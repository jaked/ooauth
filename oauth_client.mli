module type Lwt =
sig
  type 'a _r
  val bind   : 'a _r -> ('a -> 'b _r) -> 'b _r
  val return : 'a -> 'a _r
  val fail   : exn -> 'a _r
end

module Client : functor (Lwt : Lwt) ->
sig
  module type Http_client =
  sig
    val request :
      ?http_method:[ `Get | `Head | `Post ] ->
      url:string ->
      ?headers:(string * string) list ->
      ?params:(string * string) list ->
      ?body:string * string -> (* content type * body *)
      unit ->
      (Nethttp.http_status * (string * string) list * string) Lwt._r
  end

  module Make : functor (Http_client : Http_client) ->
  sig

    exception Error of Nethttp.http_status * string

    val fetch_request_token :
      ?http_method:[ `Get | `Head | `Post ] ->
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
      (string * string) Lwt._r

    val fetch_access_token :
      ?http_method:[ `Get | `Head | `Post ] ->
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
      (string * string) Lwt._r

    val access_resource :
      ?http_method:[ `Get | `Head | `Post ] ->
      url:string ->
      ?oauth_version:string ->
      ?oauth_signature_method:[ `Plaintext | `Hmac_sha1 | `Rsa_sha1 of Cryptokit.RSA.key ] ->
      oauth_consumer_key:string ->
      oauth_consumer_secret:string ->
      ?oauth_token:string ->
      ?oauth_token_secret:string ->
      ?oauth_timestamp:float ->
      ?oauth_nonce:string ->
      ?params:(string * string) list ->
      ?headers:(string * string) list ->
      ?body:string * string -> (* content type * body *)
      unit ->
      string Lwt._r
  end
end

module Sync : Lwt with type 'a _r = 'a

include module type of Client(Sync)
