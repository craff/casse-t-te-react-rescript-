open Options
module H = Http
module S = Cohttp_eio.Server

(* taken from Cohttp.Path *)

let resolve_local_file ~docroot ~uri =
  let path = Uri.(pct_decode (path (resolve "http" (of_string "/") uri))) in
  let rel_path =
    if String.length path > 0 then String.sub path 1 (String.length path - 1)
    else ""
  in
  Filename.concat docroot rel_path

(* remove a prefix from a path, usefull from nginx proxy and alike *)
let remove_prefix path =
  let prefix_len = String.length prefix in
  let path_len = String.length path in
  if path_len < prefix_len then raise Not_found;
  for i = 0 to String.length prefix - 1 do
    if path.[i] <> prefix.[i] then raise Not_found
  done;
  String.sub path prefix_len (path_len - prefix_len)

(* zlib string compression *)
let deflate_string ?(buf_size=4096) str =
  let zlib_str = Zlib.deflate_init 4 false in
  let in_pos = ref 0 in
  let in_len = String.length str in
  let buf_out = Bytes.make buf_size ' ' in
  let stop = ref false in
  let res = Buffer.create (min buf_size (in_len / 4)) in
  while not !stop do
    let do_flush = !in_pos < in_len in
    let flush = if do_flush then Zlib.Z_NO_FLUSH else Zlib.Z_FINISH in
    let (finished, used_in, used_out) =
      Zlib.deflate_string zlib_str
        str !in_pos (in_len - !in_pos) buf_out 0 buf_size flush
    in
    stop:= finished || (used_out = 0 && !in_pos >= in_len);
    in_pos := !in_pos + used_in;
    Buffer.add_subbytes res buf_out 0 used_out
  done;
  Zlib.deflate_end zlib_str;
  Buffer.contents res

(* serving cached and compressed static file *)
let read_file filename =
  let filename = resolve_local_file ~docroot ~uri:filename in
  Printf.eprintf "reading: %s\n%!" filename;
  let open Unix in
  let stats = stat filename in
  if stats.st_kind <> S_REG then raise Not_found;
  let ch = open_in filename in
  let buf = Bytes.make stats.st_size ' ' in
  really_input ch buf 0 stats.st_size;
  (Bytes.unsafe_to_string buf, filename)

let response deflate filename str =
  let str = if deflate then deflate_string str else str in
  let (r, body) = S.text_response str in
  let ty =
    match Filename.extension filename with
    | "" -> Mime_types.map_file filename
    | s  -> Mime_types.map_extension (String.sub s 1 (String.length s - 1))
  in
  let open Http.Response in let open Http.Header in
  let headers = replace r.headers "content-type" ty in
  let headers = if deflate then replace headers "Content-Encoding" "deflate"
                else headers
  in
  ({ r with headers }, body)

let send_file =
  let cache = Hashtbl.create 128 in
  (fun deflate filename ->
    match Hashtbl.find_opt cache (deflate, filename) with
    | Some p -> Eio.Promise.await_exn p
    | None ->
      let p, r = Eio.Promise.create () in
      Hashtbl.add cache (deflate, filename) p;
      match read_file filename with
      | (v,filename) ->
          let v = response deflate filename v in
          Eio.Promise.resolve_ok r v; v
      | exception ex -> Eio.Promise.resolve_error r ex; raise ex)

(* redirection to index.html *)
let redirect_index uri =
  let uri = Uri.with_uri ~path:(Some "/cocass/index.html") uri in
  let headers = Http.Header.init () in
  let headers = Http.Header.add headers "Location" (Uri.to_string uri) in
  let resp = Http.Response.make ~status:`Permanent_redirect ~headers () in
  (resp, Cohttp_eio.Body.Empty)

(* check if deflate is allowed *)
let allow_deflate request =
  let encoding = List.map (String.split_on_char ',')
    (H.Header.get_multi
     (H.Request.headers request) "Accept-Encoding")
  in
  let encoding = List.map String.trim (List.flatten encoding) in
  List.mem "deflate" encoding
