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

type result = Undefined | Good | Bad(int,int)
// test if an equation holds in the given environment
let check : (equation,env) => result = ((e1,e2), env) => {
  switch (eval(e1,env), eval(e2,env)) {
  | (v1,v2) if v1 == v2            => Good
  | (v1,v2)                        => Bad(v1,v2)
  | exception (Not_found | BadDiv) => Undefined
  }
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
