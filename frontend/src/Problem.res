open Expr

// domain is the set of integer used for the value of the variables
type domain = Belt.Set.Int.t

type problem = {
  left : string,
  right : string,
  equation : equation,
  domain : domain
}

// solve an equation.
// Naive solve trying all possibilities. Used to test correctness
// of more complex solutions (see TestExpr.res)
let solve = problem => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let ((e1,e2) as eqn) = problem.equation
  let domain = problem.domain
  let vars = union(getVariables(e1),getVariables(e2))
  let rec fn = (solutions, domain, vars, env) => {
    switch vars->minimum {
      | None =>
        // no more variables we check if we found a solution
        switch check(eqn,env) {
        | Good  => list{env, ... solutions}
        | _     => solutions
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
let iSolve = (~maxsol=?,problem) => {
  open Belt.Set.String
  module I = Belt.Set.Int
  module M = Belt.Map.String
  let ((e1,e2) as eqn) = problem.equation
  let domain = problem.domain
  let vars = union(getVariables(e1),getVariables(e2))
  let nbSol = ref(0)
  let rec fn = (solutions, domain, vars, env) => {
    // TODO: use an heuristic to select cleverly the next variable
    switch vars->minimum {
      | None =>
        // no more variables we check if we found a solution
        switch check(eqn,env) {
        | Good  => switch maxsol {
	           | None => ()
		   | Some(n) => nbSol := nbSol.contents + 1
		                if nbSol.contents > n { raise(Abort) }
                   }
	           list{env, ... solutions}
        | _     => solutions
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

let fromString = (left,right,domain) => {
  left: left,
  right: right,
  equation: (Parser.parse(left), Parser.parse(right)),
  domain: Belt.Set.Int.fromArray(domain),
}
// The "classical" puzzle:
let classical = fromString( "a+13*b/c+d+12*e-f-11+g*h/i-10",
                            "66",
			    Belt.Array.makeBy(9,i=>i+1))
