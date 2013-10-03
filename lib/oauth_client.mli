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

  val request :
    ?http_method:[ `GET | `HEAD | `POST ] ->
    url:string ->
    ?headers:(string * string) list ->
    ?params:(string * string) list ->
    ?body:string * string -> (* content type * body *)
    unit ->
    (status * (string * string) list * string) Monad.t
end

module Make : functor (Http_client : Http_client) ->
sig

  exception Error of Http_client.status * string

  val fetch_request_token :
    ?http_method:[ `GET | `HEAD | `POST ] ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_consumer_key:string ->
    oauth_consumer_secret:string ->
    ?oauth_timestamp:float ->
    ?oauth_nonce:string ->
    ?params:(string * string) list ->
    ?headers:(string * string) list ->
    unit ->
    (string * string) Http_client.Monad.t

  val fetch_access_token :
    ?http_method:[ `GET | `HEAD | `POST ] ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_consumer_key:string ->
    oauth_consumer_secret:string ->
    oauth_token:string ->
    oauth_token_secret:string ->
    ?oauth_timestamp:float ->
    ?oauth_nonce:string ->
    ?headers:(string * string) list ->
    unit ->
    (string * string) Http_client.Monad.t

  val access_resource :
    ?http_method:[ `GET | `HEAD | `POST ] ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_consumer_key:string ->
    oauth_consumer_secret:string ->
    oauth_token:string ->
    oauth_token_secret:string ->
    ?oauth_timestamp:float ->
    ?oauth_nonce:string ->
    ?params:(string * string) list ->
    ?headers:(string * string) list ->
    ?body:string * string -> (* content type * body *)
    unit ->
    string Http_client.Monad.t

end
