open Expr

// Code for parsing expressions,
// deal with spaces and parenthesis.
// Accept positive integer constants
// Accept [a-zA-Z_][a-zA-Z0-9_]* as variables
// Accept parenthesis, +, -, *, / with usual priorities
// lack unary minus

exception Parse_error(int)

let digitAt = (pos,str) => {
  open Js.String
  switch codePointAt(pos,str) {
  | Some(n) => n - 48
  | None    => raise(Parse_error(pos))
  }
}

// return position ater the blanks, starting at pos
let rec ignoreBlank = (pos,str) => {
  open Js.String
  if charAt(pos,str) == " " { ignoreBlank(pos+1,str) } else { pos }
}

let isDigit = (c) => c >= "0" && c <= "9"
let isAlpha = (c) => (c >= "a" && c <= "z")
                  || (c >= "A" && c <= "Z")
		  || c == "_"

// parse an integer from pos. returns that integer and the position after
let parseInt = (pos,str) => {
  open Js.String
  let rec gn = (pos,n) => {
    let c = charAt(pos,str)
    if isDigit(c) {
      let d = digitAt(pos,str)
      gn(pos+1,n*10+d)
    } else { (n,pos) }
  }
  gn(pos,0)
}

// parse a variable from pos. returns that integer and the position after
let parseVar = (pos0,str) => {
  open Js.String
  let rec gn = (pos) => {
    let c = charAt(pos,str)
    if isAlpha(c) || (pos > pos0 && isDigit(c)) {
      gn(pos+1)
    } else { pos }
  }
  let pos1 = gn(pos0)
  (Js.String.substring(~from=pos0,~to_=pos1, str), pos1)
}

// parse an expression. Simple recursive algorithm using a stack.
// NOTE: a bit limited if one wants to add unary minus, we would need real
// parsing and lexing library.
let parse = str => {
  open Js.String
  let rec fn = (pos,stack,prio) => {
    let pos = ignoreBlank(pos,str)
    if pos >= length(str) { (stack,pos) } else {
    let c = charAt(pos,str)
    switch (c,stack) {
    | (c,stack) if isDigit(c) =>
       let (n, pos) = parseInt(pos,str)
       fn(pos, list{Cst(n),...stack}, prio)
    | (c,stack) if isAlpha(c) =>
       let (v, pos) = parseVar(pos,str)
       fn(pos, list{Var(v),...stack}, prio)
    | ("+",list{e1,...stack}) if prio >= Sum =>
       switch (fn (pos+1,stack,Pro)) {
       | (list{e2,...stack},pos) => fn(pos,list{Add(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("-",list{e1,...stack}) if prio >= Sum =>
       switch (fn (pos+1,stack,Pro)) {
       | (list{e2,...stack},pos) => fn(pos,list{Sub(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("*",list{e1,...stack}) if prio >= Pro =>
       switch (fn (pos+1,stack,Atm)) {
       | (list{e2,...stack},pos) => fn(pos,list{Mul(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("/",list{e1,...stack}) if prio >= Pro =>
       switch (fn (pos+1,stack,Atm)) {
       | (list{e2,...stack},pos) => fn(pos,list{Div(e1,e2),...stack},prio)
       | (_,pos)                 => raise (Parse_error(pos))}
    | ("(",stack) =>
       switch (fn (pos+1,stack,Sum)) {
       | (stack,pos) =>
          let pos = ignoreBlank(pos,str)
	  let c = charAt(pos,str)
	  if (c != ")") { raise(Parse_error(pos)) }
          fn(pos+1,stack,prio)
       }
    | _ =>
       (stack,pos)
    }
  }}
  switch fn(0,list{},Sum) {
  | (list{e},pos) if pos >= length(str) => e
  | (_,pos)                             => raise(Parse_error(pos))
  }}
