// Type for numerical expression
type rec expr =
  | Var(string)       // variable
  | Cst(int)          // constant
  | Add(expr,expr)
  | Sub(expr,expr)
  | Mul(expr,expr)
  | Div(expr,expr)

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
let rec eval = (expr, env) => {
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
                     if (mod(r1, r2) != 0) { raise(Interval.BadDiv) }
                     r1 / r2
		   }
  }}

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

// an equation is just a pair of expressions
type equation = (expr,expr)

// test if an equation holds in the give environment
let check = ((e1,e2), env) =>
  switch (eval(e1,env) == eval(e2,env)) {
  | b                            => b
  | exception (Not_found | Interval.BadDiv) => false
  }

// get the list of all variables in an expression
let rec get_variables = (expr) => {
  open Belt.Set.String
  switch expr {
  | Var(v)     => empty->add(v)
  | Cst(_)     => empty
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)
  | Div(e1,e2) => union(get_variables(e1),get_variables(e2))
  }}

// solve an equation.
// Naive solve
let solve = (eqn,domain) => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let (e1,e2) = eqn
  let vars = union(get_variables(e1),get_variables(e2))
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
  Belt.List.toArray (fn (list{},domain,vars,M.empty))
}

// solve an equation, using arithmetic interval to detect
// early failure. Basically it detects two cases:
// - The left member and right member do not overlap
// - Some division will fail
let iSolve = (eqn,domain) => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let (e1,e2) = eqn
  let vars = union(get_variables(e1),get_variables(e2))
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
          switch (domain->I.minimum, domain->I.maximum) {
	  | (Some(a), Some(b)) => {
	      let full = (a,b)
  	      switch (iEval(e1,env,full), iEval(e2,env,full)) {
              | ((a,b),(c,d)) => {
    	          if b < c || d < a { solutions }
 	          else { fn(solutions,domain,vars,env) }
		}

              | exception Interval.BadDiv => solutions }
	    }
	  | _ => fn(solutions,domain,vars,env) }
	})
      }
    }
  Belt.List.toArray (fn (list{},domain,vars,M.empty))
}
