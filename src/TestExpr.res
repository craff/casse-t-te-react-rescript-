open Expr
open HtmlExpr

// some tests for parsing
assert(toString(parse("x + 123 - z")) == "x+123-z")
assert(toString(parse("x * 123 - z")) == "x*123-z")
assert(toString(parse("x + 123 / z")) == "x+123/z")
assert(toString(parse("(x + 123) / z")) == "(x+123)/z")

// solving the given problem
let str1 = "a+13*b/c+d+12*e-f-11+g*h/i-10"
let exp1 = parse(str1)
assert(toString(exp1)==str1)
let exp2 = parse("66")
let eqn0 = (exp1, exp2)
let domain = Belt.Set.Int.fromArray([1,2,3,4,5,6,7,8,9])
let solutions = solve(eqn0, domain)
Js.log(Js.Json.stringifyAny(Belt.Array.length(solutions)))
let items = toHtml(exp1)