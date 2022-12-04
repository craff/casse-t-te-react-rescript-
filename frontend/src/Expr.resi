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

// printing as string
let toString : expr => string
let stackToString : list<expr> => string

// reuse BadDiv from Interval arithmetics
exception BadDiv = Interval.BadDiv

// evaluation
type env = Belt.Map.String.t<int>
let eval : (expr,env) => int

// evaluation of expression using interval arithmetics. It is essential to
// solving puzzle efficiently by detecting failure with some unknown variables
// The last argument is an interval for the variables not bound in env
let iEval : (expr,env,Interval.t) => Interval.t

type equation = (expr,expr)

// test if an equation holds in the given environment
type result = Undefined | Good | Bad(int,int)
let check : (equation,env) => result

// get the list of all variables in an expression
let getVariables : expr => Belt.Set.String.t
