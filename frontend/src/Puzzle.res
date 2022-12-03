open Expr

type problem = {
  left : string,
  right : string,
  equation : equation,
  domain : domain
}

let fromString = (left,right) => {
  left: left,
  right: right,
  equation: (Parser.parse(left), Parser.parse(right)),
  domain: Belt.Set.Int.fromArray(Belt.Array.makeBy(9, i => i+1)),
}
// The "classical" puzzle:
let classical = fromString( "a+13*b/c+d+12*e-f-11+g*h/i-10","66")

// replaces constant from 1 to n by variables
let putVariables : (expr,int) => expr = (expr,m) => {
  let count = ref(0)
  let var = () => {
    let n = count.contents
    count := count.contents + 1
    Var(Js.String.fromCharCode(65+n))
  }
  let rec fn = (expr) => switch expr {
    | Cst(n) => if n <= m { var() } else { expr }
    | Var(_) => expr
    | Add(e1,e2) => Add(fn(e1),fn(e2))
    | Sub(e1,e2) => Sub(fn(e1),fn(e2))
    | Mul(e1,e2) => Mul(fn(e1),fn(e2))
    | Div(e1,e2) => Div(fn(e1),fn(e2))
  }
  fn(expr)
}

// We program a sleep function (should be in the standard library of ReScript?)
let sleep : int => promise<unit> = async ms => {
  let promise = Js.Promise.make((~resolve, ~reject as _) =>
    Js.Global.setTimeout(_ => resolve (.()),ms)->ignore)
  await promise
}

let cancelled = ref(false)
let cancel = _ => cancelled := true

// generates a problem using all integer for 1 to size,
// integer from 1 to m, must be guessed by the player
// optionally, maxsol is the maximum number of solutions
// callback maybe used to display the number of problems explored
let generate : (~maxsol:int=?, ~callback:(int=>unit)=?,int,int)
               => promise<option<problem>>
             = async (~maxsol=?,~callback=?,size,m) => {
  let domain = Belt.Set.Int.fromArray(Belt.Array.makeBy(m, (i => i+1)))
  // we choose a random order of the integer from 1 to size
  let domainArray = Belt.Array.makeBy(size, i => i+1)
  Belt.Array.shuffleInPlace(domainArray)

  // Need async not to block the navigator...
  let do_one : unit => option<equation> = () => {
    // build the problem as a string that alternates operation and integers.
    // size integers = size - 1 oprations
    // So we need an array of length 2*size - 1
    let a = Belt.Array.makeBy(2*size - 1, (i =>
    // even positions correspondond to integer
    if mod(i,2) == 0 {
       // this is a number
       Belt.Int.toString(domainArray[i/2])
    } else {
       // division have proba 2/12, mult 3/12, sub 3/12 and add 4/12
       // like the original problem
       let x = 12.0 *. Js.Math.random()
       if (x < 4.0) { "+" }
       else if (x < 7.0) { "-" }
       else if (x < 10.0) { "*" }
       else { "/" }
    }))
    // we parse the generated expression
    let e = Parser.parse(Js.String.concatMany(a,""))
    try {
      // and check if it is well defined and compute its value.
      let r = eval(e, Belt.Map.String.empty)
      // reject negative values
      if r < 0 { None }
      else {
        let e = putVariables(e,m)
        let eqn = (e, Cst(r))
        switch maxsol {
        | None     => Some(eqn)
        | Some(nb) =>
	  // maxsol was given, we check if the solution is acceptable
	  let _ = iSolve(~maxsol=nb,eqn,domain); { Some(eqn) }
        }
      }
    }
    catch { | Interval.BadDiv | Abort => None }
  }
  // loop using async and sleep until we find an acceptable problems
  let count = ref(0)
  let rec fn = async () =>
    switch (cancelled.contents, do_one ()) {
    | (true, _)      => cancelled := false; None
    | (_, Some(eqn)) => // Hourah: we found a solution
       Some(eqn)
    | (_, None)      =>
       count := count.contents + 1
       switch callback {
       | None => ()
       | Some(f) => f(count.contents)
       }
       await sleep(30)
       await fn()
  }
  await fn() |> res => switch res {
    | None => None
    | Some((e1,e2) as equation) =>
      Some({ left:toString(e1), right:toString(e2),equation,domain})
    }
}