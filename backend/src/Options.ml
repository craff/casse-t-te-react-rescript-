let prefix = ref ""
let port = ref 8080
let maxc = ref 5
let static_dir = ref ""

let spec =
  Arg.[
      ("-p", Set_string prefix,
       "fix the prefix of the url (no \"/\" allowed) (defaut \"\")");
      ("-P", Set_int port, "the port to listen to (default 8080)");
      ("-d", Set_string static_dir, "the location of static files (default \"\")");
      ("-m", Set_int maxc, "maximum allowed connections (default 5)");
  ]

let usage = Printf.sprintf "usage: %s [-]" Sys.argv.(0)

let anon_fun s =
  Printf.eprintf "Dot not know what to do with %s\n%!" s;
  exit 1

let () = Arg.parse spec anon_fun usage

let prefix = !prefix
let port = !port
let maxc = !maxc
let static_dir = !static_dir
