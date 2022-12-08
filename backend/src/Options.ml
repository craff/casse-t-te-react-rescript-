let prefix = ref ""
let port = ref 8080
let domains = ref (Domain.recommended_domain_count ())
let max_pool_size = ref 3
let maxc = ref 5
let docroot = ref ""
let addr = ref "127.0.0.1"

let spec =
  Arg.[
      ("-p", Set_string prefix,
       "fix the prefix of the url (no \"/\" allowed) (defaut \"\")");
      ("-P", Set_int port, "the port to listen to (default 8080)");
      ("-a", Set_string addr, "the address to listen to (default 127.0.0.1)");
      ("-d", Set_string docroot, "the location of static files (default \"\")");
      ("-n", Set_int domains, "number of thread/domains used (default recommended_domain_count)");
      ("-d", Set_int max_pool_size, "maximum number of db connections per domain (default 3)");
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
let docroot = !docroot
let addr = !addr
let prefix = if prefix <> "" && prefix.[0] <> '/' then "/" ^ prefix else prefix
let domains = !domains
let max_pool_size = !max_pool_size
