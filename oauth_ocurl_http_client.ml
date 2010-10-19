(* Ocamlnet Http_client doesn't support SSL *)

let request
    ?(http_method = `Post)
    ~url
    ?(headers = [])
    ?(params = [])
    ?body
    () =
  let h = Buffer.create 1024 in
  let b = Buffer.create 1024 in

  let oc = Curl.init() in
  let query = Netencoding.Url.mk_url_encoded_parameters params in
  let headers =
    match http_method, body with
      | `Post, None ->
          Curl.set_url oc url;
          Curl.set_postfields oc query;
          Curl.set_postfieldsize oc (String.length query);
          headers
      | `Post, Some (content_type, body) ->
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          Curl.set_url oc url;
          Curl.set_postfields oc body;
          Curl.set_postfieldsize oc (String.length body);
          ("Content-type", content_type)::headers
      | `Get, _ | `Head, _ ->
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          Curl.set_url oc url;
          headers in
  Curl.set_headerfunction oc (fun s -> Buffer.add_string h s; String.length s);
  Curl.set_writefunction oc (fun s -> Buffer.add_string b s; String.length s);
  if List.length headers > 0
  then begin
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) headers in
    Curl.set_httpheader oc headers;
  end;
(*
  Curl.set_proxy oc "localhost";
  Curl.set_proxyport oc 9888;
  Curl.set_sslverifypeer oc false;
*)
  Curl.set_transfertext oc true;
  Curl.perform oc;
  Curl.cleanup oc;

  (* adapted from Ocamlnet http_client.ml *)
  try
    let line_end_re = Netstring_pcre.regexp "[^\\x00\r\n]+\r?\n" in
    let line_end2_re = Netstring_pcre.regexp "([^\\x00\r\n]+\r?\n)*\r?\n" in
    let status_re = Netstring_pcre.regexp "^([^ \t]+)[ \t]+([0-9][0-9][0-9])([ \t]+([^\r\n]*))?\r?\n$" in

    let c = Buffer.contents h in
    let code, in_pos =
      (* Parses the status line. If 1XX: do XXX *)
      match Netstring_pcre.string_match line_end_re c 0 with
        | None -> raise (Failure "couldn't parse status")
        | Some m ->
            let s = Netstring_pcre.matched_string m c in
            match Netstring_pcre.string_match status_re s 0 with
              | None -> raise (Failure "Bad status line")
              | Some m ->
                  let code_str = Netstring_pcre.matched_group m 2 s in
                  let code = int_of_string code_str in
                  if code < 100 || code > 599
                  then raise (Failure "Bad status code")
                  else Nethttp.http_status_of_int code, Netstring_pcre.match_end m in

    let header =
      (* Parses the HTTP header following the status line *)
      match Netstring_pcre.string_match line_end2_re c in_pos with
        | None -> raise (Failure "couldn't parse header")
        | Some m ->
            let start = in_pos in
            let in_pos = Netstring_pcre.match_end m in
            let header_l, _ =
              Mimestring.scan_header
                ~downcase:false ~unfold:true ~strip:true c
                ~start_pos:start ~end_pos:in_pos in
            header_l in

    (code, header, Buffer.contents b)

  with Failure msg -> (`Internal_server_error, [], msg)
