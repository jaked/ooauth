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

module Make : functor (Http_client : Http_client) ->
sig
  exception Error of Http_client.status_code * string

  val fetch_temporary_credentials :
    ?http_method:Http_client.meth ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_client:string ->
    oauth_client_secret:string ->
    ?oauth_timestamp:float ->
    ?oauth_nonce:string ->
    ?params:(string * string) list ->
    ?headers:(string * string) list ->
    unit ->
    (string * string) Http_client.Monad.t

  val fetch_token_credentials :
    ?http_method:Http_client.meth ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_client:string ->
    oauth_client_secret:string ->
    oauth_token:string ->
    oauth_token_secret:string ->
    ?oauth_timestamp:float ->
    ?oauth_nonce:string ->
    ?headers:(string * string) list ->
    unit ->
    (string * string) Http_client.Monad.t

  val access_resource :
    ?http_method:Http_client.meth ->
    url:string ->
    ?oauth_version:string ->
    ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
    oauth_client:string ->
    oauth_client_secret:string ->
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
