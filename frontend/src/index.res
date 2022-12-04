// Main code for the web page

// A general react component for a button
module Button = {
   @react.component
    let make = (~onClick, ~text) =>
    <button onClick> {React.string(text)} </button>
}

// A react component for the input holding the maximum number
// of solutions when creating new problems
// the parameter get is a reference to a function of type unit => int
// that will give the current value
// This allows to leek state from components.
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
// Same trick as above to set the text using set : ref(string => unit)
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
let body   = {
  open ReactDOM
  switch querySelector("body") {
  | None    => assert(false)
  | Some(e) => Client.createRoot(e)
  }
}

// the text holding the various message of the page
let (centerElt, setCenter) = {
  let set = ref (_ => assert false)
  let elt = <Text init="" id="centerText" set/>
  (elt, n => set.contents(Js.Int.toString(n)))
}

// Setting the variable associated to the initial problem
let currentProblem = ref(Problem.classical)
let currentId      = ref(-1) // set in initPuzzle below
let currentInputs  = ref(Js.Dict.empty())

// Set the url link to the current puzzle
let setLink = id => {
  // TODO: is there a cleaner way in Rescript
  open ReactDOM
  let port = %raw(`window.location.port`)
  let url = %raw(`window.location.protocol`) ++
            "//" ++ %raw(`window.location.hostname`) ++
	    ":" ++ port ++
            %raw(`window.location.pathname`) ++ "?id=" ++ Belt.Int.toString(id)
  let span = switch querySelector("#link") {
  | None    => assert(false)
  | Some(e) => Client.createRoot(e)
  }
  span->ReactDOM.Client.Root.render(React.string(url))
}

// main function creating the initial puzzle
let initPuzzle = _ => {
  let problem = currentProblem.contents
  // The html for the puzzle
  // - puzzle if the html element
  // - inputs is a dictionnary holding the user's solution
  let (puzzle,inputs) = HtmlExpr.toHtml(problem.equation)
  // This send the default pb in the rare case it is not in the data base
  // and fetch is id to update the link.
  Api.sendProblem(problem, id => {currentId:=id; setLink(id)})
  currentInputs := inputs
  puzzle
}

// main function changing the puzzle
let setPuzzle = _ => {
  let problem = currentProblem.contents
  // The html for the puzzle
  // - puzzle if the html element
  // - inputs is a dictionnary holding the user's solution
  let (puzzle,inputs) = HtmlExpr.toHtml(problem.equation)
  currentInputs := inputs
  let div   = {
    open ReactDOM
    switch querySelector("#puzzle") {
    | None    => assert(false)
    | Some(e) => Client.createRoot(e)
    }
  }
  div->ReactDOM.Client.Root.render(puzzle)
}

// get the initial problem from the id in url. If no id is given:
// we keep the initial problem. No Rescript bind so we use raw javascript
%%raw(`
const queryParameters = new URLSearchParams(window.location.search)
`)
let requestProblemId : Js.Nullable.t<int> = %raw(`queryParameters.get("id")`)
let requestProblemId = Js.Nullable.toOption(requestProblemId)
switch (requestProblemId) {
  | Some(id) =>
    let setProblemCb   = (pb,id) => {
      currentProblem := pb
      currentId      := id
      setLink(id)
      setPuzzle()
    }
    Api.getProblem(id,setProblemCb)
  | None => ()
}

// the text holding the various message of the page
let (resultElt, setResult) = {
  let set = ref (_ => assert false)
  let elt = <Text id="result" init="" set/>
  (elt, text => set.contents(text))
}

// callback to the solve puzzle
let solvePuzzle = (_event) => {
  let problem = currentProblem.contents
  // Currently all puzzles use the same domain as the "classical one"
  let t0 = Js.Date.make()
    let solutions = Problem.iSolve(problem)
    let t1 = Js.Date.make()
    let nb = Belt.Array.length(solutions)
    let dt = Js.Date.getTime(t1) -. Js.Date.getTime(t0)
    let text = Belt.Int.toString(nb) ++ " " ++ Lang.solutions_found ++ " " ++
               Belt.Float.toString(dt) ++ "ms"
    setResult(text)
    // We choose a random solution to give to the user
    let solution = solutions[Js.Math.random_int(0,nb)]
    // We send that solution to the server
    Api.sendSolution(solution,true,currentId.contents)
    solution->Belt.Map.String.forEach((k,x) =>
      switch(currentInputs.contents->Js.Dict.get(k)) {
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
      let env : Belt.Map.String.t<int> = Js.Dict.entries(currentInputs.contents)->
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
      if not (Expr.check(currentProblem.contents.equation,env)) {
        raise(Bad(Lang.not_good))
      }
      Api.sendSolution(env,false,currentId.contents)
      setResult(Lang.good_solution)
    } catch {
      | Bad(msg) => setResult(Lang.bad_solution ++ ": " ++ msg)
    }
  }

// a ref to a function to get the maximum number of solutions
// this ref is updated by the input element
let getMaxSol = ref (() => 50)

// callback to search for a new puzzle
// TODO : create a "cancel" button
let newPuzzle = (_event) => {
  open Generate
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
        | Some(problem) =>
	  currentProblem:=problem;
	  Api.sendProblem(problem, id => {currentId:=id; setLink(id)})
	  setPuzzle()
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
let elt = {
  <div id="main"><div id="overlay">
    <div id="center">
      <div>{centerElt}</div>
      <div><Button onClick={Generate.cancel} text=Lang.cancel/></div>
    </div>
  </div>
  <div id ="root">
    <h1>{React.string(Lang.title)}</h1>
    <p id="rule">{React.string(Lang.description)}</p>
    <div className="header">
      <Button onClick=check text=Lang.check_solution/>
      <Button onClick=newPuzzle text=Lang.generate/>
      <MaxSol init=50 get=getMaxSol/>
      <Button onClick=solvePuzzle text=Lang.solve/>
    </div>
    <div id="puzzle">{initPuzzle()}</div>
    <div className="footer">
      {resultElt}
    </div>
    <p>{React.string(Lang.link)}<span id="link"></span></p>
  </div>
  </div>}

// add the element to the page
let _ = body->ReactDOM.Client.Root.render(elt)
