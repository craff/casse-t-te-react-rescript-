
module S = Tiny_httpd
module D = Tiny_httpd_dir

let () = Db.init()

let fail msg e =
  Printf.eprintf "%s: %s\n%!" msg (Printexc.to_string e);
  S.Response.fail ~code:500 "%s:%s" msg (Printexc.to_string e)

(* API requests *)
let send_problem req =
  try
    let problem = Yojson.Basic.from_string(req.S.Request.body) in
    let id = Db.add_problem problem in
    S.Response.make_string (Ok (string_of_int id))
  with e -> fail "Error in send_problem" e

let get_problem req =
  try
    let query = S.Request.query req in
    let id = List.assoc "id" query in
    let pb = Db.get_problem id in
    S.Response.make_string (Ok pb)
  with e -> fail "Error in get_problem" e

let send_solution req =
  try
    let solution = Yojson.Basic.from_string(req.S.Request.body) in
    Db.add_solution solution;
    S.Response.make_string (Ok "ok")
  with e -> fail "Error in send_solution" e

open Options

let pexact str =
  let open S.Route in
  if prefix = "" then exact str @/ return
  else exact prefix @/ exact str @/ return

let () =
  let server = S.create ~max_connections:Options.maxc ~port:Options.port () in
  (* serving frontend directory *)
  let dir = "../frontend/dist" in
  let config = D.config () in (* default config is what we want *)
  D.add_dir_path ~config ~dir ~prefix server;
  (* serving API requests *)
  S.add_route_handler server (pexact "send_problem") send_problem;
  S.add_route_handler server (pexact "get_problem") get_problem;
  S.add_route_handler server (pexact "send_solution") send_solution;
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
