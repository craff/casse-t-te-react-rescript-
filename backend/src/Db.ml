open Lwt.Infix

let db =
  Caqti_lwt.connect
    (Uri.of_string
       "postgresql://cocass:63SrhmCmVkSxbAg@localhost")
  >>= Caqti_lwt.or_fail |> Lwt_main.run

let init () =
  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS problem (
	   id SERIAL PRIMARY KEY,
	   created timestamp NOT NULL DEFAULT current_timestamp,
	   "left" text NOT NULL,
	   "right" text NOT NULL,
	   domain int[] NOT NULL)
         |sql}]
  in

  query () db >>= Caqti_lwt.or_fail |> Lwt_main.run;

  let query =
    [%rapper execute
        {sql|
         CREATE TYPE binding as (var text, val int)
         |sql}]
  in

  (* Type may already exists and IF NOT EXIST is not supported *)
  (try query () db >>= Caqti_lwt.or_fail |> Lwt_main.run with _ -> ());

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS solution (
	   id SERIAL PRIMARY KEY,
	   created timestamp NOT NULL DEFAULT current_timestamp,
	   problem int NOT NULL,
	   auto boolean NOT NULL,
	   env binding[] NOT NULL,
           CONSTRAINT problem_id
             FOREIGN KEY(problem)
             REFERENCES problem(id))
         |sql}]
  in

  query () db >>= Caqti_lwt.or_fail |> Lwt_main.run;

  ()

let get_string lbl s =
  match s with
  | `String s -> s
  | _       -> failwith ("error: " ^ lbl)

let get_int lbl s =
  match s with
  | `Int s -> s
  | _       -> failwith ("error: " ^ lbl)

let get_bool lbl s =
  match s with
  | `Bool s -> s
  | _       -> failwith ("error: " ^ lbl)

module IntArray : Rapper.CUSTOM with type t = int array = struct
  type t = int array

  let t =
    let encode a =
      let r = ref "{" in
      Array.iteri (fun i x -> r := !r ^ (if i = 0 then "" else ",") ^ string_of_int x)
        a;
      Ok (!r ^ "}")
    in
    let decode str =
      let%parser parser =
        '{' '}' => []
      ; '{' (n::INT) (ns:: ~* (',' (n::INT) => n)) '}' => n::ns
      in
      let open Pacomb in
      let blank = Blank.from_charset (Charset.singleton ' ') in
      try
        let l = Grammar.parse_string parser blank str in
        Ok (Array.of_list(l))
      with Pos.Parse_error(_) ->
	Error ("parse_error")
    in
    Caqti_type.(custom ~encode ~decode string)
end

module Env : Rapper.CUSTOM with type t = (string * int) array = struct
  type t = (string * int) array

  let t =
    let encode a =
      let r = ref "{" in
      Array.iteri (fun i (k,x) ->
	r := !r ^ (if i = 0 then "" else ",") ^ "\"(" ^ k ^ "," ^ string_of_int x ^ ")\"")
        a;
      Ok (!r ^ "}")
    in
    let decode str =
      let%parser tuple =  '(' (k::STRING_LIT) ',' (n::INT) ')' => (k,n) in
      let%parser parser =
        '{' '}' => []
      ; '{' (t::tuple) (ts:: ~* (',' (t::tuple) => t)) '}' => t::ts
      in
      let open Pacomb in
      let blank = Blank.from_charset (Charset.singleton ' ') in
      try
        let l = Grammar.parse_string parser blank str in
        Ok (Array.of_list(l))
      with Pos.Parse_error(_) ->
	Error ("parse_error")
    in
    Caqti_type.(custom ~encode ~decode string)

end

let add_problem problem =
  match problem with
  | `Assoc l ->
     let left = get_string "problem(left)" (List.assoc "left" l) in
     let right = get_string "problem(right)" (List.assoc "right" l) in
     let domain = List.assoc "domain" l in
     let domain = match domain with
                  | `List l -> Array.of_list (List.map (get_int "problem(domain)") l)
		  | _       -> failwith "bad problem(domain)"
     in
     Array.sort compare domain;
     let query = [%rapper get_opt
              {sql|SELECT @int{id} FROM problem
                   WHERE "left"=%string{left} AND
	                 "right"=%string{right} AND
		         "domain"=%IntArray{domain}
              |sql}]
     in
     let result = query ~left ~right ~domain db >>= Caqti_lwt.or_fail |> Lwt_main.run in
     begin
       match result with
       | Some x -> x
       | _      ->
       let query = [%rapper get_one
              {sql|INSERT INTO problem ("left", "right", domain)
                   VALUES (%string{left}, %string{right}, %IntArray{domain})
                   RETURNING @int{id}
              |sql}]
       in
       query ~left ~right ~domain db >>= Caqti_lwt.or_fail |> Lwt_main.run
     end
  | _ -> failwith "bad problem"

let get_problem id =
  let id = int_of_string id in
  let query = [%rapper get_opt
     {sql|SELECT @string{"left"}, @string{"right"}, @IntArray{domain}
          FROM problem WHERE id=%int{id} |sql}]
  in
  let result = query ~id db >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | None -> failwith "no problem with this id"
  | Some (left,right,domain) ->
    Array.sort compare domain;
    let domain = Array.to_list domain in
    let json : Yojson.Basic.t
       = `Assoc [("left",`String left);
                 ("right",`String right);
                 ("domain",`List (List.map (fun x -> `Int x) domain))]
    in
    let res = Yojson.Basic.to_string json in
    res

let add_solution (solution:Yojson.Basic.t) =
  try match solution with
  | `Assoc l ->
     let env = List.assoc "env" l in
     let problem = get_int "solution(problem)" (List.assoc "problem" l) in
     let auto = get_bool "solution(auto)" (List.assoc "auto" l) in
     let env = match env with
       | `Assoc a ->
          List.fold_left (fun acc (k, x) -> (k, get_int "solution(env)" x) :: acc) [] a
       | _ -> raise Exit
     in
     let env = Array.of_list env in
     let query = [%rapper execute
              {sql|INSERT INTO solution (problem, auto, env)
                   VALUES (%int{problem}, %bool{auto}, %Env{env})
              |sql}]
     in
     query ~problem ~auto ~env db >>= Caqti_lwt.or_fail |> Lwt_main.run
  | _ -> ()
  with Exit -> failwith "bad solution"
