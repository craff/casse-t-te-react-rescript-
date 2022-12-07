open Http_util
module H = Http
module S = Cohttp_eio.Server

let () = Db.init()

(* API requests *)
let send_problem _params body =
  let problem = match body with
  | Some body -> Yojson.Basic.from_string body
  | None -> raise Not_found
  in
  let id = Db.add_problem problem in
  S.text_response (string_of_int id)

let get_problem params _body =
  match List.assoc "id" params with
  | []      -> raise Not_found
  | id :: _ ->
    let pb = Db.get_problem id in
    S.text_response pb

let send_solution _params body =
  let solution = match body with
  | Some body -> Yojson.Basic.from_string body
  | None -> raise Not_found
  in
  Db.add_solution solution;
  S.text_response "ok"

open Options

let domains = Domain.recommended_domain_count ()

let () =
  let handler (request,buf,_addr) =
    try
      Format.eprintf "%a\n%!" H.Request.pp request;
      let uri = Uri.of_string request.resource in
      let path = remove_prefix (Uri.path uri) in
      let uri = Uri.with_uri ~path:(Some path) uri in
      let params = Uri.query uri in
      let deflate = allow_deflate request in
      Printf.eprintf "deflate: %b\n%!" deflate;
      let body = S.read_fixed request buf in
      match path with
      | "/send_problem" -> send_problem params body
      | "/get_problem" -> get_problem params body
      | "/send_solution" -> send_solution params body
      | "" | "/" -> redirect_index uri
      | _ -> send_file deflate uri
    with _ -> S.not_found_response
  in

  let f () =
    try Eio_main.run @@ fun env -> S.run ~domains ~port env handler
    with e -> Printf.eprintf "unexpected toplevel exception: %s"
                   (Printexc.to_string e)
  in
  while true do
    Printf.eprintf "start listening\n%!";
    f ()
  done
