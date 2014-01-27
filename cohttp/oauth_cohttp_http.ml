module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

let (>>=) = Lwt.bind

module Monad = Lwt

type status_code = int
type meth = C.Code.meth

type request = CU.Server.Request.t

exception Error of status_code * string

let http_method = CU.Request.meth

let url req = Uri.to_string (CU.Request.uri req)

let header req h =
  let hs = CU.Request.headers req in
  match C.Header.get hs (String.lowercase h) with
  | Some h -> h
  | None -> raise Not_found

let argument req ?default arg =
  let uri = CU.Request.uri req in
  match default, Uri.get_query_param uri arg with
  | _, Some v -> v
  | Some d, None -> d
  | _ -> raise Not_found

let arguments req =
  let uri = CU.Request.uri req in
  List.map (fun (k,vs) -> (k, List.hd vs)) (Uri.query uri)

type response = (CU.Response.t * Cohttp_lwt_body.t)

let respond req status headers body =
  let headers = C.Header.of_list headers in
  let status = C.Code.status_of_code status in
  CU.Server.respond_string ~headers ~status ~body ()
