module type Http_client =
sig
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end

  type status =
    [ `Accepted
    | `Bad_gateway
    | `Bad_request
    | `Code of int
    | `Conflict
    | `Continue
    | `Created
    | `Expectation_failed
    | `Forbidden
    | `Found
    | `Gateway_time_out
    | `Gone
    | `HTTP_version_not_supported
    | `Internal_server_error
    | `Length_required
    | `Method_not_allowed
    | `Moved_permanently
    | `Multiple_choices
    | `No_content
    | `Non_authoritative_information
    | `Not_acceptable
    | `Not_found
    | `Not_implemented
    | `Not_modified
    | `OK
    | `Partial_content
    | `Payment_required
    | `Precondition_failed
    | `Proxy_authentication_required
    | `Request_URI_too_large
    | `Request_entity_too_large
    | `Request_time_out
    | `Requested_range_not_satisfiable
    | `Reset_content
    | `See_other
    | `Service_unavailable
    | `Switching_protocols
    | `Temporary_redirect
    | `Unauthorized
    | `Unprocessable_entity
    | `Unsupported_media_type
    | `Use_proxy ]

  type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

  val request :
    ?http_method:meth ->
    url:string ->
    ?headers:(string * string) list ->
    ?params:(string * string) list ->
    ?body:string * string -> (* content type * body *)
    unit ->
    (status * (string * string) list * string) Monad.t
end

module Make (Http_client : Http_client) =
struct

  open Http_client.Monad

  exception Error of Http_client.status * string

  open Oauth_common



  let authorization_header
      ~oauth_version ~oauth_signature_method ~oauth_signature
      ~oauth_consumer_key ?oauth_token
      ~oauth_timestamp ~oauth_nonce
      () =
    let params =
      [
        "OAuth realm", "";
        "oauth_version", oauth_version;
        "oauth_signature_method", string_of_signature_method oauth_signature_method;
        "oauth_signature", oauth_signature;
        "oauth_consumer_key", oauth_consumer_key;
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
      | _ -> raise (Error (`Internal_server_error, "bad response: " ^ res))



  let fetch_request_token
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_consumer_key ~oauth_consumer_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?params ?(headers = [])
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_timestamp ~oauth_nonce
        ?params
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_consumer_key
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

    Http_client.request
      ~http_method
      ~url
      ~headers
      ?params
      () >>= function
    | (`OK, _, res) -> return (parse_response res)
    | (status, _, res) -> fail (Error (status, res))



  let fetch_access_token
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_consumer_key ~oauth_consumer_secret
      ~oauth_token ~oauth_token_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?(headers = [])
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_token ~oauth_token_secret
        ~oauth_timestamp ~oauth_nonce
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_consumer_key ~oauth_token
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

      Http_client.request
        ~http_method
        ~url
        ~headers
        () >>= function
      | (`OK, _, res) -> return (parse_response res)
      | (status, _, res) -> fail (Error (status, res))



  let access_resource
      ?(http_method = `POST) ~url
      ?(oauth_version = "1.0") ?(oauth_signature_method = `Hmac_sha1)
      ~oauth_consumer_key ~oauth_consumer_secret
      ~oauth_token ~oauth_token_secret
      ?(oauth_timestamp = make_timestamp ()) ?(oauth_nonce = make_nonce ())
      ?params ?(headers = []) ?body
      () =

    let oauth_signature =
      sign
        ~http_method ~url
        ~oauth_version ~oauth_signature_method
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_token ~oauth_token_secret
        ~oauth_timestamp ~oauth_nonce
        ?params
        () in

    let headers =
      authorization_header
        ~oauth_version ~oauth_signature_method ~oauth_signature
        ~oauth_consumer_key ~oauth_token
        ~oauth_timestamp ~oauth_nonce
        () :: headers in

      Http_client.request
        ~http_method
        ~url
        ~headers
        ?params
        ?body
        () >>= function
      | (`OK, _, res) -> return res
      | (status, _, res) -> fail (Error (status, res))

end
