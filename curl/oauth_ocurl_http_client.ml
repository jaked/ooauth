(* Ocamlnet Http_client doesn't support SSL *)

let status_of_code = function
  | 100 -> `Continue
  | 101 -> `Switching_protocols
  | 200 -> `OK
  | 201 -> `Created
  | 202 -> `Accepted
  | 203 -> `Non_authoritative_information
  | 204 -> `No_content
  | 205 -> `Reset_content
  | 206 -> `Partial_content
  | 300 -> `Multiple_choices
  | 301 -> `Moved_permanently
  | 302 -> `Found
  | 303 -> `See_other
  | 304 -> `Not_modified
  | 305 -> `Use_proxy
  | 307 -> `Temporary_redirect
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 402 -> `Payment_required
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | 406 -> `Not_acceptable
  | 407 -> `Proxy_authentication_required
  | 408 -> `Request_time_out
  | 409 -> `Conflict
  | 410 -> `Gone
  | 411 -> `Length_required
  | 412 -> `Precondition_failed
  | 413 -> `Request_entity_too_large
  | 414 -> `Request_URI_too_large
  | 415 -> `Unsupported_media_type
  | 416 -> `Requested_range_not_satisfiable
  | 417 -> `Expectation_failed
  | 422 -> `Unprocessable_entity
  | 500 -> `Internal_server_error
  | 501 -> `Not_implemented
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 504 -> `Gateway_time_out
  | 505 -> `HTTP_version_not_supported
  | code -> `Code code

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
  let query = Uri.encoded_of_query (List.map (fun (k,v) -> (k, [v])) params) in
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
    let open Re_pcre in
    let line_end_re = regexp "[^\\x00\r\n]+\r?\n" in
    let line_end2_re = regexp "([^\\x00\r\n]+\r?\n)*\r?\n" in
    let status_re = regexp "^([^ \t]+)[ \t]+([0-9][0-9][0-9])([ \t]+([^\r\n]*))?\r?\n$" in

    let string_match rex s pos =
      try
        let result = Re_pcre.exec ~rex ~pos s in
        Some result
      with Not_found -> None
    in

    let matched_group result n _ = Re_pcre.get_substring result n in
    let matched_string result _ = Re_pcre.get_substring result 0 in
    let match_end result = snd (Re_pcre.get_substring_ofs result 0) in

    let c = Buffer.contents h in
    let code, in_pos =
      (* Parses the status line. If 1XX: do XXX *)
      match string_match line_end_re c 0 with
        | None -> raise (Failure "couldn't parse status")
        | Some m ->
            let s = matched_string m c in
            match string_match status_re s 0 with
              | None -> raise (Failure "Bad status line")
              | Some m ->
                  let code_str = matched_group m 2 s in
                  let code = int_of_string code_str in
                  if code < 100 || code > 599
                  then raise (Failure "Bad status code")
                  else status_of_code code, match_end m in

    let header =
      (* Parses the HTTP header following the status line *)
      match string_match line_end2_re c in_pos with
        | None -> raise (Failure "couldn't parse header")
        | Some m ->
            let start = in_pos in
            let in_pos = match_end m in
            let header_l, _ =
              Mimestring.scan_header
                ~downcase:false ~unfold:true ~strip:true c
                ~start_pos:start ~end_pos:in_pos in
            header_l in

    (code, header, Buffer.contents b)

  with Failure msg -> (`Internal_server_error, [], msg)
