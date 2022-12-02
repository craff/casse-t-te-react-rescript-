open Expr
open Parser

// some tests for parsing
assert(toString(parse("x + 123 - z")) == "x+123-z")
assert(toString(parse("x * 123 - z")) == "x*123-z")
assert(toString(parse("x + 123 / z")) == "x+123/z")
assert(toString(parse("(x + 123) / z")) == "(x+123)/z")

// solving the classical problem
let str1 = "a+13*b/c+d+12*e-f-11+g*h/i-10"
let exp1 = parse(str1)
let exp2 = parse("66")
// test parser
assert(toString(exp1)==str1)

// testing both version of solve

let test = (eqn0,n) => {
  let domain = Belt.Set.Int.fromArray(Belt.Array.makeBy(n, (i => i+1)))
  let t0 = Js.Date.getTime(Js.Date.make())
  let solutions = solve(eqn0, domain)
  let t1 =  Js.Date.getTime(Js.Date.make())
  let t2 = Js.Date.getTime(Js.Date.make())
  let solutions2 = iSolve(eqn0, domain)
  let t3 = Js.Date.getTime(Js.Date.make())
  let (e,r) = eqn0
  Js.log(toString(e) ++ "=" ++ toString(r))
  Js.log(Belt.Int.toString(Belt.Array.length(solutions)) ++ " " ++ Belt.Float.toString(t1-.t0))
  Js.log(Belt.Int.toString(Belt.Array.length(solutions2)) ++ " " ++ Belt.Float.toString(t3-.t2))
  Belt.SortArray.stableSortInPlaceBy(solutions,compare)
  Belt.SortArray.stableSortInPlaceBy(solutions2,compare)
  assert(solutions==solutions2)
}

test((exp1,exp2),9)

let multiTest = (~maxsol=?,nb,size,m) => {
  open Puzzle
  for _ in 1 to nb {
    generate(~maxsol=?maxsol,size,m)->Js.Promise.then_(pb => {
      test(pb.equation,m)
      Js.Promise.resolve(())
      },_)->ignore
  }
}

multiTest(20,4,2)
multiTest(20,8,5)
multiTest(~maxsol=50,2,13,9)
multiTest(20,13,9)
