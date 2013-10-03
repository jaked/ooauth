module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

let (>>=) = Lwt.bind

type status = C.Code.status

module Opt = struct
  type 'a t = 'a option
  let (>>=) x f = match x with Some v -> f v | None -> None
  let (>|=) x f = match x with Some v -> Some (f v) | None -> None
  let run x = match x with Some v -> v | None -> raise (Invalid_argument "run")
  let default d x = match x with Some v -> v | None -> d
end

module Monad = Lwt

let request ?http_method ~url ?headers ?params ?body () =
  let uri = Uri.of_string url in
  let uri = match params with
    | Some p -> Uri.with_query' uri p
    | None -> uri
  in
  CU.Client.call
    ?headers:Opt.(headers >|= fun hs -> C.Header.of_list hs)
    ?body:Opt.(body >>= fun b -> CB.body_of_string b)
    Opt.(default `GET http_method)
    uri
    >>= function
      | None -> Lwt.fail (Failure "Connection did not succeed")
      | Some (response, body) ->
        let status = CU.Response.status response in
        let headers = C.Header.to_list (CU.Response.headers response) in
        CB.string_of_body body >>= fun body_string ->
        Lwt.return (status, headers, body_string)
