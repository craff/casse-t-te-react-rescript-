
let domain = Belt.Set.Int.fromArray([1,2,3,4,5,6,7,8,9])

module Button = {
   @react.component
    let make = (~onClick, ~text) =>
    <button onClick> {React.string(text)} </button>
}
module MaxSol = {
   @react.component
   let make = (~get,~init) => {
     let (text, setText) = React.useState(_ => Belt.Int.toString(init));
     let onChange = evt => {
       ReactEvent.Form.preventDefault(evt)
       let value = ReactEvent.Form.target(evt)["value"]
       setText(_prev => value);
     }
     let elt = <input onChange id="maxsol" value=text type_="number"/>
     get := (() => switch Belt.Int.fromString(text) {
                   | None    => 50
		   | Some(n) => n })
     elt
   }
}


@val external enableAll: unit => unit = "enableAll"
@val external disableAll: unit => unit = "disableAll"

exception Bad(string)

let rootElt   = ReactDOM.querySelector("#root")

let rec setPuzzle = ((exp1,exp2) as eqn) => {
  let (puzzle,inputs) = HtmlExpr.toHtml(exp1,exp2)

  let setResult = text => {
    switch ReactDOM.querySelector("#result") {
    | Some(elt) => ReactDOM.render(React.string(text),elt)
    | None => () // do nothing
  }}

  let setCenter = nb => {
    switch ReactDOM.querySelector("#center") {
    | Some(elt) => ReactDOM.render(React.string(Js.Int.toString(nb) ++ " problèmes testés"),elt)
    | None => () // do nothing
  }}

  let solvePuzzle = (_event) => {
    let t0 = Js.Date.make()
    let solutions = Expr.iSolve(eqn,domain)
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

  let getMaxSol = ref (() => 50)

  let newPuzzle = (_event) => {
    disableAll()
    let count = ref(0)
    let callback = (nb) => {setCenter(count.contents); count := nb}
    let t0 = Js.Date.getTime(Js.Date.make())
    let maxsol = getMaxSol.contents()
    Js.Global.setTimeout((() => Puzzle.generate(~maxsol,~callback,13,9)->Js.Promise.then_(eqn => {
      setPuzzle(eqn)
      enableAll()
      let t1 = Js.Date.getTime(Js.Date.make())
      setResult(Js.Int.toString(count.contents) ++ " problèmes testés en " ++
                   Js.Float.toString(t1 -. t0) ++ "ms")
      Js.Promise.resolve(())
    }, _) -> ignore), 10)->ignore
  }

  let elt =
    <div>
      <div className="header">
        <Button onClick=check text="teste ma solution"/>
        <Button onClick=newPuzzle text="genère un nouveau problème avec un nombre de solution \u2264 "/>
	<MaxSol init=50 get=getMaxSol/>
        <Button onClick=solvePuzzle text="résoud automatiquement"/>
      </div>
      {puzzle}
      <div className="footer">
        <span id="result"></span>
      </div>
    </div>

  switch rootElt {
    | Some(root) => ReactDOM.render(elt,root)
    | None => () // do nothing
  }
}

let str1 = "a+13*b/c+d+12*e-f-11+g*h/i-10"
let exp1 = Parser.parse(str1)

let str2 = "66"
let exp2 = Parser.parse(str2)
let eqn  = (exp1, exp2)

setPuzzle(eqn)
