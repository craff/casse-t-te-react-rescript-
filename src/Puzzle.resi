open Expr

type problem = {
  left : string,
  right : string,
  equation : equation,
  domain : domain
}

// The "classical" puzzle:
let classical : problem

let generate : (~maxsol:int=?, ~callback:(int=>unit)=?,int,int)
               => promise<option<problem>>

// cancel current generation
let cancel : 'a => unit