
module S = Tiny_httpd
module D = Tiny_httpd_dir

let () =
  let server = S.create () in
  (* serving frontend directory *)
  let dir = "../frontend/dist" in
  let config = D.config () in (* default config is what we want *)
  D.add_dir_path ~config ~dir ~prefix:"" server;
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
