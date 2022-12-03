// Type for numerical expression
type rec expr =
  | Var(string)       // variable
  | Cst(int)          // constant
  | Add(expr,expr)
  | Sub(expr,expr)
  | Mul(expr,expr)
  | Div(expr,expr)

// Priority when printing and parsing expressions
type prio = Atm | Pro | Sum

// To debug, we need a way to translate expression to String
let toString = (expr) => {
  open Js.String
  let rec fn = (expr,prio) => {
    let paren = (priol,prior,e1,e2,symb) =>
      if priol > prio {
        concatMany(["(",fn(e1,priol),symb,fn(e2,prior),")"],"")
      } else {
        concatMany([fn(e1,priol),symb,fn(e2,prior)],"")
      }
    switch expr {
    | Var(v) => v
    | Cst(n) => Belt.Int.toString(n)
    | Add(e1,e2) => paren(Sum,Sum,e1,e2,"+")
    | Sub(e1,e2) => paren(Sum,Pro,e1,e2,"-")
    | Mul(e1,e2) => paren(Pro,Pro,e1,e2,"*")
    | Div(e1,e2) => paren(Pro,Atm,e1,e2,"/")
    }}
  fn(expr,Sum)
  }

// The same for list of expressions used during parser
let stackToString = (list) => {
  list->Belt.List.reduce("", (acc, e) =>
    Js.String.concat(acc,toString(e)))
  }

// evaluation of an expresion using a dictionnay to give the value of variables

type env = Belt.Map.String.t<int>

// reuse BadDiv from Interval arithmetics
exception BadDiv = Interval.BadDiv

let rec eval = (expr, env:env) => {
  open Belt.Map.String
  switch expr {
   | Var(v)     => switch get(env,v) {
                   | None    => raise(Not_found)
                   | Some(n) => n }
   | Cst(n)     => n
   | Add(e1,e2) => eval(e1, env) + eval(e2, env)
   | Sub(e1,e2) => eval(e1, env) - eval(e2, env)
   | Mul(e1,e2) => eval(e1, env) * eval(e2, env)
   | Div(e1,e2) => { let r1 = eval(e1, env) and r2 = eval(e2, env)
                     if (mod(r1, r2) != 0) { raise(BadDiv) }
                     r1 / r2
		   }
  }}

// evaluation of expression using interval arithmetics. It is essential to
// solving puzzle efficiently by detecting failure with some unknown variables
let iEval = (expr, env, full) => {
  open Interval
  open Belt.Map.String
  let rec eval = expr => {
  switch expr {
   | Var(v)     => switch get(env,v) {
                   | None    => full
                   | Some(n) => (n,n) }
   | Cst(n)     => (n,n)
   | Add(e1,e2) => iAdd(eval(e1), eval(e2))
   | Sub(e1,e2) => iSub(eval(e1), eval(e2))
   | Mul(e1,e2) => iMul(eval(e1), eval(e2))
   | Div(e1,e2) => iDiv(eval(e1), eval(e2))
  }}
  eval(expr)}

type equation = (expr,expr)

// test if an equation holds in the given environment
let check : (equation,env) => bool = ((e1,e2), env) =>
  switch (eval(e1,env) == eval(e2,env)) {
  | b                            => b
  | exception (Not_found | BadDiv) => false
  }

// get the list of all variables in an expression
let rec getVariables : expr => Belt.Set.String.t = (expr) => {
  open Belt.Set.String
  switch expr {
  | Var(v)     => empty->add(v)
  | Cst(_)     => empty
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)
  | Div(e1,e2) => union(getVariables(e1),getVariables(e2))
  }}

// domain is the set of integer used for the value of the variables
type domain = Belt.Set.Int.t

// solve an equation.
// Naive solve trying all possibilities. Used to test correctness
// of more complex solutions (see TestExpr.res)
let solve = (eqn,domain) => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let (e1,e2) = eqn
  let vars = union(getVariables(e1),getVariables(e2))
  let rec fn = (solutions, domain, vars, env) => {
    switch vars->minimum {
      | None =>
        // no more variables we check if we found a solution
        switch check(eqn,env) {
        | true  => list{env, ... solutions}
        | false => solutions
        | exception Not_found => assert(false)
        }
      | Some(v) =>
        // we try all remaining value in the domain
        let vars = vars->remove(v)
        domain->I.reduce(solutions, (solutions,x) => {
	  let domain = domain->I.remove(x)
	  let env = env->M.set(v,x)
	  fn(solutions,domain,vars,env)
	})
      }
    }
  // convert the list of solutions to array
  Belt.List.toArray (fn (list{},domain,vars,M.empty))
}

exception Abort

// solve an equation, using arithmetic interval to detect
// early failure. Basically it detects two cases:
// - The left member and right member do not overlap
// - Some division will fail
// - we interrupt with exception Abort is maxsol is given
//   this is used when building problems to avoid searching for
//   all solutions.
let iSolve = (~maxsol=?,eqn,domain) => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let (e1,e2) = eqn
  let vars = union(getVariables(e1),getVariables(e2))
  let nbSol = ref(0)
  let rec fn = (solutions, domain, vars, env) => {
    // TODO: use an heuristic to select cleverly the next variable
    switch vars->minimum {
      | None =>
        // no more variables we check if we found a solution
        switch check(eqn,env) {
        | true  => switch maxsol {
	           | None => ()
		   | Some(n) => nbSol := nbSol.contents + 1
		                if nbSol.contents > n { raise(Abort) }
                   }
	           list{env, ... solutions}
        | false => solutions
        | exception Not_found => assert(false)
        }
      | Some(v) =>
        // we try all remaining value in the domain
        let vars = vars->remove(v)
        domain->I.reduce(solutions, (solutions,x) => {
	  let domain = domain->I.remove(x)
	  let env = env->M.set(v,x)
	  // We evaluate the expression in the interval holding
	  // the remaining value
          switch (domain->I.minimum, domain->I.maximum) {
	  | (Some(a), Some(b)) => {
	      let full = (a,b)
	      // Check is left and right interval agree
  	      switch (iEval(e1,env,full), iEval(e2,env,full)) {
              | ((a,b),(c,d)) => {
    	          if b < c || d < a { solutions }
 	          else { fn(solutions,domain,vars,env) }
		}

              | exception BadDiv => solutions }
	    }
	  // No more variables we check our solution via
	  // the recursive call
	  | _ => fn(solutions,domain,vars,env) }
	})
      }
    }
  // convert the list of solutions to array
  Belt.List.toArray (fn (list{},domain,vars,M.empty))
}
