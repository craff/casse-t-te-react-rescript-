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
   | Div(e1,e2) => eval(e1, env) / eval(e2, env)
  }}

// an equation is just a pair of expressions
type equation = (expr,expr)

// test if an equation holds in the give environment
let check = ((e1,e2), env) =>
  switch (eval(e1,env) == eval(e2,env)) {
  | b                   => b
  | exception Not_found => false
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
// TODO: This is a dumb algorithm trying all possibilities.
//       One way to improve is detect symmetries in solutions using
//       commutativity and associativity
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

// Code for parsing expressions,
// deal with spaces and parenthesis.
// Accept positive integer constants
// Accept [a-zA-Z_][a-zA-Z0-9_]* as variables
// Accept parenthesis, +, -, *, / with usual priorities
// lack unary minus

exception Parse_error(int)

let digitAt = (pos,str) => {
  open Js.String
  switch codePointAt(pos,str) {
  | Some(n) => n - 48
  | None    => raise(Parse_error(pos))
  }
}

// return position ater the blanks, starting at pos
let rec ignoreBlank = (pos,str) => {
  open Js.String
  if charAt(pos,str) == " " { ignoreBlank(pos+1,str) } else { pos }
}

let isDigit = (c) => c >= "0" && c <= "9"
let isAlpha = (c) => (c >= "a" && c <= "z")
                  || (c >= "A" && c <= "Z")
		  || c == "_"

// parse an integer from pos. returns that integer and the position after
let parseInt = (pos,str) => {
  open Js.String
  let rec gn = (pos,n) => {
    let c = charAt(pos,str)
    if isDigit(c) {
      let d = digitAt(pos,str)
      gn(pos+1,n*10+d)
    } else { (n,pos) }
  }
  gn(pos,0)
}

// parse a variable from pos. returns that integer and the position after
let parseVar = (pos0,str) => {
  open Js.String
  let rec gn = (pos) => {
    let c = charAt(pos,str)
    if isAlpha(c) || (pos > pos0 && isDigit(c)) {
      gn(pos+1)
    } else { pos }
  }
  let pos1 = gn(pos0)
  (Js.String.substring(~from=pos0,~to_=pos1, str), pos1)
}

// parse an expression. Simple recursive algorithm using a stack.
// NOTE: a bit limited if one wants to add unary minus, we would need real
// parsing and lexing library.
let parse = (str) => {
  open Js.String
  let rec fn = (pos,stack,prio) => {
    let pos = ignoreBlank(pos,str)
    if pos >= length(str) { (stack,pos) } else {
    let c = charAt(pos,str)
    switch (c,stack) {
    | (c,stack) if isDigit(c) =>
       let (n, pos) = parseInt(pos,str)
       fn(pos, list{Cst(n),...stack}, prio)
    | (c,stack) if isAlpha(c) =>
       fn(pos+1, list{Var(c),...stack}, prio)
    | ("+",list{e1,...stack}) if prio >= Sum =>
       switch (fn (pos+1,stack,Pro)) {
       | (list{e2,...stack},pos) => fn(pos,list{Add(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("-",list{e1,...stack}) if prio >= Sum =>
       switch (fn (pos+1,stack,Pro)) {
       | (list{e2,...stack},pos) => fn(pos,list{Sub(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("*",list{e1,...stack}) if prio >= Pro =>
       switch (fn (pos+1,stack,Atm)) {
       | (list{e2,...stack},pos) => fn(pos,list{Mul(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("/",list{e1,...stack}) if prio >= Pro =>
       switch (fn (pos+1,stack,Atm)) {
       | (list{e2,...stack},pos) => fn(pos,list{Div(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("(",stack) =>
       switch (fn (pos+1,stack,Sum)) {
       | (stack,pos) =>
          let pos = ignoreBlank(pos,str)
	  let c = charAt(pos,str)
	  if (c != ")") { raise(Parse_error(pos)) }
          fn(pos+1,stack,prio)
       }
    | _ =>
       (stack,pos)
    }
  }}
  switch fn(0,list{},Sum) {
  | (list{e},pos) if pos >= length(str) => e
  | (stack,pos)                         => Js.log(stack); raise(Parse_error(pos))
  }}
