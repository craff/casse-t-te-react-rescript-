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
let check : (equation,env) => bool

// get the list of all variables in an expression
let get_variables : expr => Belt.Set.String.t

// domain is the set of integer used for the value of the variables
type domain = Belt.Set.Int.t

// solve an equation.
// Naive solve trying all possibilities. Used to test correctness
// of more complex solutions (see TestExpr.res)
// It returns all solutions
let solve : (equation,domain) => array<env>

// solve an equation, using arithmetic interval to detect
// early failure. Basically it detects two cases:
// - The left member and right member do not overlap
// - Some division will fail
// - we interrupt with exception Abort is maxsol is given
//   this is used when building problems to avoid searching for
//   all solutions.
exception Abort
let iSolve : (~maxsol:int=?, equation, domain) => array<env>
