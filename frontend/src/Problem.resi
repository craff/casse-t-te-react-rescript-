open Expr

// domain is the set of integer used for the value of the variables
type domain = Belt.Set.Int.t

type problem = {
  left : string,
  right : string,
  equation : equation,
  domain : domain
}

// The "classical" puzzle:
let classical : problem

// solve an equation.
// Naive solve trying all possibilities. Used to test correctness
// of more complex solutions (see TestExpr.res)
// It returns all solutions
let solve : problem => array<env>

// solve an equation, using arithmetic interval to detect
// early failure. Basically it detects two cases:
// - The left member and right member do not overlap
// - Some division will fail
// - we interrupt with exception Abort is maxsol is given
//   this is used when building problems to avoid searching for
//   all solutions.
exception Abort
let iSolve : (~maxsol:int=?, problem) => array<env>

let fromString : (string,string,array<int>) => problem