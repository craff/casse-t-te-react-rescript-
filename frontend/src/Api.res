
type json_problem = {
   left : string,
   right : string,
   domain : array<int>
}

let readOption = (x) => switch x { | Some(x) => x | None => assert(false) }

let parseIntoInt: Js.Json.t => int = json =>
  switch Js.Json.decodeNumber(json) {
  | Some(n) => Belt.Float.toInt(n)
  | None    => assert(false)
}

let parseIntoProblem = {
  open Js.Json
  json => switch classify(readOption(json)) {
  | JSONObject(dict) =>
    let left = switch dict->Js.Dict.get("left") {
    | Some(s) => switch classify(s) {
      | JSONString(s) => s
      | _ => assert false
      }
    | _ => assert false
    }
    let right = switch dict->Js.Dict.get("right") {
    | Some(s) => switch classify(s) {
      | JSONString(s) => s
      | _ => assert false
      }
    | _ => assert false
    }
    let domain = switch dict->Js.Dict.get("domain") {
    | Some(s) => switch classify(s) {
      | JSONArray(s) => s
      | _ => assert false
      }
    | _ => assert false
    }
    let domain = domain->Belt.Array.map(parseIntoInt)
//    let domain = domain->Belt.Array.reduce(Belt.Set.Int.empty,(acc,n) =>
//      acc->Belt.Set.Int.add(parseIntoInt(n)))
    Problem.fromString (left,right,domain)
  | _ => assert false
  }
}

let getProblem = (id,cb) => {
   open Request
   let url = Js.Global.encodeURI ("get_problem?id="++Belt.Int.toString(id))
   make(~url, ~method=#GET, ~responseType=Json, ())->Future.get(json => {
     switch json {
     | Error(_) => ()
     | Ok(resp) => if resp.ok {
         let pb = parseIntoProblem(resp.response)
         cb(pb,id)
       }
     }
   })
}

let sendProblem = (problem,cb) => {
   let domain = problem.Problem.domain->Belt.Set.Int.reduce(list{},
     (acc,i) => list{i,...acc})
   let domain = Belt.List.toArray(domain)
   let problem = {
     left: problem.Problem.left,
     right: problem.Problem.right,
     domain: domain
   }
   let json = Js.Json.stringifyAny(problem)
   switch json {
   | None => assert false
   | Some(json) =>
     let url = Js.Global.encodeURI ("send_problem")
     open Request
     make(~url, ~method=#POST, ~body=json,
                  ~responseType=Json, ())->Future.get(json => {
       switch json {
       | Error(_) => ()
       | Ok(resp) => if resp.ok {let id = parseIntoInt(readOption(resp.response));
                     Js.log("problem id = " ++ Belt.Int.toString(id));
		     cb(id)}
       }
     })
   }
}

let sendSolution = (env,auto,id) => {
   let dictEnv = Js.Dict.empty()
   env->Belt.Map.String.forEach((k,v) => dictEnv->Js.Dict.set(k, v))
   let dict = { "env":dictEnv, "problem":id, "auto":auto }
   let json = Js.Json.stringifyAny(dict)
   switch json {
   | None => assert false
   | Some(json) =>
     let url = Js.Global.encodeURI ("send_solution")
     open Request
     make(~url, ~method=#POST, ~body=json,
                  ~responseType=Json, ())->Future.get(json => {
       switch json {
       | Error(_) => ()
       | Ok(resp) => if resp.ok {Js.log("solution saved")}
       }
     })
   }
}
