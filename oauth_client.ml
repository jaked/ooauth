module type Http_client =
sig
  val request :
    ?http_method:[ `Get | `Head | `Post ] ->
    url:string ->
    ?headers:(string * string) list ->
    ?params:(string * string) list ->
    ?body:string * string -> (* content type * body *)
    unit ->
    Nethttp.http_status * (string * string) list * string
end

module Make (Http_client : Http_client) =
struct

  exception Error of Nethttp.http_status * string

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
      let params = Netencoding.Url.dest_url_encoded_parameters res in
      (List.assoc "oauth_token" params, List.assoc "oauth_token_secret" params)
    with
      | _ -> raise (Error (`Internal_server_error, "bad response: " ^ res))



  let fetch_request_token
      ?(http_method = `Post) ~url
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

    let res =
      Http_client.request
        ~http_method
        ~url
        ~headers
        ?params
        () in

    match res with
      | (`Ok, _, res) -> parse_response res
      | (status, _, res) -> raise (Error (status, res))



  let fetch_access_token
      ?(http_method = `Post) ~url
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

    let res =
      Http_client.request
        ~http_method
        ~url
        ~headers
        () in

    match res with
      | (`Ok, _, res) -> parse_response res
      | (status, _, res) -> raise (Error (status, res))



  let access_resource
      ?(http_method = `Post) ~url
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

    let res =
      Http_client.request
        ~http_method
        ~url
        ~headers
        ?params
        ?body
        () in

    match res with
      | (`Ok, _, res) -> res
      | (status, _, res) -> raise (Error (status, res))

end
