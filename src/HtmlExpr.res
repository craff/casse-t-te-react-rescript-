open Expr

module Input = {
  @react.component
  let make = (~id,~dict) => {
     let (text, setText) = React.useState(_ => "");
     let onChange = evt => {
       ReactEvent.Form.preventDefault(evt)
       let value = ReactEvent.Form.target(evt)["value"]
       setText(_prev => value);
     }
     let elt = <input onChange value=text type_="text" id/>
     let get = () => text
     let set = x => setText(_ => Belt.Int.toString(x))
     dict->Js.Dict.set(id,(set,get))
     elt
  }
}

let genKey = {
  let count = ref(0)
  let f = () => { count := count.contents+1; "key" ++ Js.Int.toString(count.contents) }
  f
}

// Convert expression to list of strings
let toStringArray = (~inputs=?,expr) => {
  open Belt.Array
  let inputs = switch inputs {
    | None => Js.Dict.empty()
    | Some(d) => d
  }
  let rec fn = (expr,prio) => {
    let paren = (priol,prior,e1,e2,symb) => {
      let symb = <div key={genKey()} className="cell">{React.string(symb)}</div>
      if priol > prio {
        let openPar = <div key={genKey()} className="cell">{React.string("(")}</div>
        let closePar = <div key={genKey()} className="cell">{React.string(")")}</div>
        concatMany([[openPar],fn(e1,priol),[symb],fn(e2,prior),[closePar]])
      } else {
        concatMany([fn(e1,priol),[symb],fn(e2,prior)])
      }
    }
    switch expr {
    | Var(id)    => [<div key={id} className="cell">{<Input id dict=inputs/>} </div>]
    | Cst(n)     => [<div key={genKey()} className="cell">{React.string(Belt.Int.toString(n))}</div>]
    | Add(e1,e2) => paren(Sum,Sum,e1,e2,"+")
    | Sub(e1,e2) => paren(Sum,Pro,e1,e2,"-")
    | Mul(e1,e2) => paren(Pro,Pro,e1,e2,"*")
    | Div(e1,e2) => paren(Pro,Atm,e1,e2,"/")
    }
  }
  let elt = fn(expr,Sum)
  (elt,inputs)
}

let toHtml = (expr1,expr2) => {
  let (expr1,inputs) = toStringArray(expr1)
  let (expr2,inputs) = toStringArray(~inputs,expr2)

  let eq = <div key={genKey()} className="cell">{React.string("=")}</div>

  let elt = <div className="expr"> {React.array(Belt.Array.concatMany([expr1,[eq],expr2]))} </div>
  (elt, inputs)
}
