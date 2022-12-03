open Expr

// The input are dictionnay with a set and get function used to
// deal with the input fields in the problem to solve.
// the get fonction return a string to handle the empty/error cases
type inputs = Js.Dict.t<(int => unit, unit => string)>

// transform an equation into an element + the inputs
let toHtml : equation => (React.element, inputs)