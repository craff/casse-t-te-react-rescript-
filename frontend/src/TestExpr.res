open Expr
open Problem
open Parser

// some tests for parsing
assert(toString(parse("x + 123 - z")) == "x+123-z")
assert(toString(parse("x * 123 - z")) == "x*123-z")
assert(toString(parse("x + 123 / z")) == "x+123/z")
assert(toString(parse("(x + 123) / z")) == "(x+123)/z")

// testing both version of solve

let test = problem => {
  let t0 = Js.Date.getTime(Js.Date.make())
  let solutions = solve(problem)
  let t1 =  Js.Date.getTime(Js.Date.make())
  let t2 = Js.Date.getTime(Js.Date.make())
  let solutions2 = iSolve(problem)
  let t3 = Js.Date.getTime(Js.Date.make())
  Js.log(problem.left ++ "=" ++ problem.right)
  Js.log(Belt.Int.toString(Belt.Array.length(solutions)) ++ " " ++ Belt.Float.toString(t1-.t0))
  Js.log(Belt.Int.toString(Belt.Array.length(solutions2)) ++ " " ++ Belt.Float.toString(t3-.t2))
  Belt.SortArray.stableSortInPlaceBy(solutions,compare)
  Belt.SortArray.stableSortInPlaceBy(solutions2,compare)
  assert(solutions==solutions2)
}

test(Problem.classical)->ignore

let multiTest = (~maxsol=?,nb,size,m) => {
  open Generate
  for _ in 1 to nb {
    generate(~maxsol=?maxsol,size,m)->Js.Promise.then_(pb => {
      switch pb {
      | None => assert false
      | Some(pb) => test(pb)
      }
      Js.Promise.resolve(())
      },_)->ignore
  }
}

multiTest(20,4,2)
multiTest(20,8,5)
multiTest(~maxsol=50,2,13,9)
multiTest(20,13,9)
