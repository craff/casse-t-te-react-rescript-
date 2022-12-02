// Main code for the web page

// A general react component for a button
module Button = {
   @react.component
    let make = (~onClick, ~text) =>
    <button onClick> {React.string(text)} </button>
}

// A react component for the input holding the maximum number
// of solutions when creating new problems
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

// A module for modifialble text.
// We use references to lift the state outside the component
// It seems to work, may may not be legal.
module Text = {
   @react.component
   let make = (~init,~set,~id) => {
     let (text, setText) = React.useState(_ => init);
     set := (text => setText(_ => text))
     <span id>{React.string(text)}</span>
  }
}

// We import for functions from index.html to set the display property of the
// overlay div hidding the whole page
@val external enableAll: unit => unit = "enableAll"
@val external disableAll: unit => unit = "disableAll"

// exception raised when the solution proposed by the user is wrong
exception Bad(string)

// get the root element of the page
let rootElt   = {
  open ReactDOM
  switch querySelector("#root") {
  | None => assert(false)
  | Some(root) => Client.createRoot(root)
  }
}

// get the center element of the overlay
let overlayElt   = {
  open ReactDOM
  switch querySelector("#overlay") {
  | None => assert(false)
  | Some(root) => Client.createRoot(root)
  }
}

// the text holding the various message of the page
let (centerElt, setCenter) = {
  let set = ref (_ => assert false)
  let elt = <Text init="" id="centerText" set/>
  (elt, n => set.contents(Js.Int.toString(n)))
}

overlayElt->ReactDOM.Client.Root.render(
  <div id="center" >
    <div>{centerElt}</div>
    <div><Button onClick={Puzzle.cancel} text=Lang.cancel/></div>
  </div>
 )

// main function creating the puzzle
let rec setPuzzle = problem => {
  open Puzzle
  // the html for the puzzle
  // - puzzle if the html element
  // - inputs is a dictionnary holding the user's solution
  let (puzzle,inputs) = HtmlExpr.toHtml(problem.equation)

  // the text holding the various message of the page
  let (resultElt, setResult) = {
    let set = ref (_ => assert false)
    let elt = <Text id="result" init="" set/>
    (elt, text => set.contents(text))
  }

  // callback to the solve puzzle
  let solvePuzzle = (_event) => {
    // Currently all puzzles use the same domain as the "classical one"
    let domain = classical.domain
    let t0 = Js.Date.make()
    let solutions = Expr.iSolve(problem.equation,domain)
    let t1 = Js.Date.make()
    let nb = Belt.Array.length(solutions)
    let dt = Js.Date.getTime(t1) -. Js.Date.getTime(t0)
    let text = Belt.Int.toString(nb) ++ " " ++ Lang.solutions_found ++ " " ++
               Belt.Float.toString(dt) ++ "ms"
    setResult(text)
    // We choose a random solution to give to the user
    let solution = solutions[Js.Math.random_int(0,nb)]
    solution->Belt.Map.String.forEach((k,x) =>
      switch(inputs->Js.Dict.get(k)) {
        | Some(set,_) => set(x)
        | None => () // do nothing
      }
    )
  }

  // callback to the "test solution" button
  let check = (_event) => {
    open Belt.Map.String
    // an array to check that all integers are used
    let used = Belt.Array.make(9,false)
    try {
      // we scan the user inputs to produce an environment
      let env : Belt.Map.String.t<int> = Js.Dict.entries(inputs)->
          Belt.Array.reduce(empty,(env,(k,(_,get))) => {
        switch(Belt.Int.fromString(get())) {
        | None    =>
	  // not all inputs are integer
	  raise(Bad(Lang.not_integer))
        | Some(x) =>
          if x < 1 || x > 9 { raise(Bad(Lang.bad_interval)) }
	  used[x-1] = true
          env->set(k,x)
        }
      })
      if not (Belt.Array.every(used, (x => x))) {
        raise(Bad(Lang.not_all))
      }
      if not (Expr.check(problem.equation,env)) {
        raise(Bad(Lang.not_good))
      }
      setResult(Lang.good_solution)
    } catch {
      | Bad(msg) => setResult(Lang.bad_solution ++ ": " ++ msg)
    }
  }

  // a ref to a function to get the maximum number of solutions
  let getMaxSol = ref (() => 50)

  // callback to search for a new puzzle
  // TODO : create a "cancel" button
  let newPuzzle = (_event) => {
    disableAll()
    let count = ref(0)
    let callback = (nb) => {setCenter(count.contents); count := nb}
    let t0 = Js.Date.getTime(Js.Date.make())
    let maxsol = getMaxSol.contents()
    // Puzzle.generate is a promise that call timeout not to block the
    // navigator.
    generate(~maxsol,~callback,13,9)->Js.Promise.then_(problem => {
      switch problem {
        | None => ()
        | Some(problem) => setPuzzle(problem)
      }
      enableAll()
      let t1 = Js.Date.getTime(Js.Date.make())
      setResult(Js.Int.toString(count.contents) ++ " " ++
                Lang.nb_tested ++ " " ++
                Js.Float.toString(t1 -. t0) ++ "ms")
      Js.Promise.resolve(())
    }, _) -> ignore
  }

  // The root element!
  let elt =
    <div>
      <h1>{React.string(Lang.title)}</h1>
      <p id="rule">{React.string(Lang.description)}</p>
      <div className="header">
        <Button onClick=check text=Lang.check_solution/>
        <Button onClick=newPuzzle text=Lang.generate/>
	<MaxSol init=50 get=getMaxSol/>
        <Button onClick=solvePuzzle text=Lang.solve/>
      </div>
      {puzzle}
      <div className="footer">
        {resultElt}
      </div>
    </div>

  // add the element to the page
  rootElt->ReactDOM.Client.Root.render(elt)
}

// Create the initial puzzle
setPuzzle(Puzzle.classical)

Js.log(Lang.all)