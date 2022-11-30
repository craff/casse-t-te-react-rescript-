
let domain = Belt.Set.Int.fromArray([1,2,3,4,5,6,7,8,9])

let str1 = "a+13*b/c+d+12*e-f-11+g*h/i-10"
let exp1 = Expr.parse(str1)

let str2 = "66"
let exp2 = Expr.parse(str2)
let eqn  = (exp1, exp2)
let (puzzle,inputs) = HtmlExpr.toHtml(exp1,exp2)

let setResult = text =>
  switch(ReactDOM.querySelector("#result")){
    | Some(elt) => ReactDOM.render(React.string(text),elt)
    | None => () // do nothing
  }

let solve_puzzle = (_event) => {
  let t0 = Js.Date.make()
  let solutions = Expr.solve(eqn,domain)
  let nb = Belt.Array.length(solutions)
  let t1 = Js.Date.make()
  let dt = Js.Date.getTime(t1) -. Js.Date.getTime(t0)
  let text = Belt.Int.toString(nb) ++ " solutions found in " ++ Belt.Float.toString(dt) ++ "ms"
  setResult(text)
  let solution = solutions[Js.Math.random_int(0,nb)]
  solution->Belt.Map.String.forEach((k,x) =>
    switch(inputs->Js.Dict.get(k)) {
      | Some(set,_) => set(x)
      | None => () // do nothing
    }
  )
}

module Button = {
   @react.component
    let make = (~onClick, ~text) =>
    <button onClick> {React.string(text)} </button>
}

exception Bad(string)

let check = (_event) => {
  open Belt.Map.String
  let used = Belt.Array.make(9,false)
  try {
    let env = Js.Dict.entries(inputs)->Belt.Array.reduce(empty,(env,(k,(_,get))) => {
      switch(Belt.Int.fromString(get())) {
      | None    => raise(Bad("pas des entiers"))
      | Some(x) =>
        if x < 1 || x > 9 { raise(Bad("pas entre 1 et 9")) }
	used[x-1] = true
        env->set(k,x)
      }
    })
    if not (Belt.Array.every(used, (x => x))) { raise(Bad("il manque des entiers de 1 à 9")) }
    if not (Expr.check(eqn,env)) { raise(Bad("pas le bon resultat")) }
    setResult("bonne solution")
  } catch {
  | Bad(msg) => setResult("mauvaise solution: " ++ msg)
  }
}

let elt =
  <div>
    <div className="header">
      <Button onClick=check text="teste ma solution"/>
      <Button onClick=solve_puzzle text="résoud automatiquement"/>
      <span id="result"></span>
    </div>
    {puzzle}
  </div>

// Dom access can actually fail. ReScript
// is really explicit about handling edge cases!
Js.log("START")
switch(ReactDOM.querySelector("#root")){
    | Some(root) => ReactDOM.render(elt,root)
    | None => () // do nothing
}
