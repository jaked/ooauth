module type Http_client =
sig
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end

  type status_code = int
  type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

  val request :
    ?http_method:meth ->
    url:string ->
    ?headers:(string * string) list ->
    ?params:(string * string) list ->
    ?body:string * string -> (* content type * body *)
    unit ->
    (status_code * (string * string) list * string) Monad.t
end

module Make (Http_client : Http_client) =
struct

  open Http_client.Monad

  exception Error of Http_client.status_code * string

  open Oauth_common

  let authorization_header
      ~oauth_version ~oauth_signature_method ~oauth_signature
      ~oauth_client ?oauth_token
      ~oauth_timestamp ~oauth_nonce
      () =
    let params =
      [
        "OAuth realm", "";
        "oauth_version", oauth_version;
        "oauth_signature_method", string_of_signature_method oauth_signature_method;
        "oauth_signature", oauth_signature;
        "oauth_consumer_key", oauth_client;
        "oauth_timestamp", string_of_timestamp oauth_timestamp;
        "oauth_nonce", oauth_nonce;
      ] @
        opt_param "oauth_token" oauth_token in

    "Authorization",
    (params |>
        List.map (fun (k, v) -> k ^ "=\"" ^ String.escaped (rfc3986_encode v) ^ "\"") |>
            String.concat ",")



  let parse_response res =
    try
      let params = Uri.query_of_encoded res |> List.map (fun (k,vs) -> k,List.hd vs) in
      (List.assoc "oauth_token" params, List.assoc "oauth_token_secret" params)
    with
      | _ -> raise (Error (500, "bad response: " ^ res))



  let fetch_temporary_credentials
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_client ~oauth_client_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?params ?(headers = [])
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_client ~oauth_client_secret
        ~oauth_timestamp ~oauth_nonce
        ?params
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_client
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

    Http_client.request
      ~http_method
      ~url
      ~headers
      ?params
      () >>= function
    | (200, _, res) -> return (parse_response res)
    | (status, _, res) -> fail (Error (status, res))



  let fetch_token_credentials
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_client ~oauth_client_secret
      ~oauth_token ~oauth_token_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?(headers = [])
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_client ~oauth_client_secret
        ~oauth_token ~oauth_token_secret
        ~oauth_timestamp ~oauth_nonce
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_client ~oauth_token
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

      Http_client.request
        ~http_method
        ~url
        ~headers
        () >>= function
      | (200, _, res) -> return (parse_response res)
      | (status, _, res) -> fail (Error (status, res))



  let access_resource
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_client ~oauth_client_secret
      ~oauth_token ~oauth_token_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?params ?(headers = []) ?body
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_client ~oauth_client_secret
        ~oauth_token ~oauth_token_secret
        ~oauth_timestamp ~oauth_nonce
        ?params
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_client ~oauth_token
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

      Http_client.request
        ~http_method
        ~url
        ~headers
        ?params
        ?body
        () >>= function
      | (200, _, res) -> return res
      | (status, _, res) -> fail (Error (status, res))

end
