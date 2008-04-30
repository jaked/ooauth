module type Http_client =
sig
  val request :
    [ `Get | `Head | `Post ] ->
    string ->
    (string * string) list -> (* headers *)
    (string * string) list -> (* params *)
    Nethttp.http_status * (string * string) list * string
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
    ?other_params:(string * string) list ->
    unit ->
    string * string

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
    unit ->
    string * string

  val access_resource :
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
    ?other_params:(string * string) list ->
    unit ->
    string

end
