module type Http =
sig
  type request
  val http_method : request -> [ `Get | `Post | `Head ]
  val url : request -> string
  val header : request -> string -> string (* throws Not_found *)
  val argument : request -> ?default:string -> string -> string (* throws Not_found *)
  val arguments : request -> (string * string) list

  type response
  val respond : request -> Nethttp.http_status -> (string * string) list -> string -> response

  exception Error of Nethttp.http_status * string
end

module type Db =
sig
  module Http : Http

  type consumer
  val lookup_consumer : string -> consumer (* throws Not_found *)
  val consumer_key : consumer -> string
  val consumer_secret : consumer -> string
  val consumer_rsa_key : consumer -> Cryptokit.RSA.key (* throws Not_found *)

  type request_token
  val make_request_token : consumer -> Http.request -> request_token
  val lookup_request_token: string -> request_token (* throws Not_found *)
  val request_token_check_consumer : request_token -> consumer -> bool
  val request_token_token : request_token -> string
  val request_token_secret : request_token -> string
  val request_token_authorized : request_token -> bool
  val authorize_request_token : request_token -> Http.request -> unit (* throws Failure *)

  type access_token
  val exchange_request_token : request_token -> access_token (* throws Failure *)
  val lookup_access_token : string -> access_token (* throws Not_found *)
  val access_token_check_consumer : access_token -> consumer -> bool
  val access_token_token : access_token -> string
  val access_token_secret : access_token -> string
end

module Make :
  functor (Http : Http) ->
    functor (Db : Db with module Http = Http) ->
sig

  val fetch_request_token : Http.request -> Http.response

  val fetch_access_token : Http.request -> Http.response

  val authorize_request_token :
    Http.request ->
    (string -> Db.request_token -> Http.request -> Http.response) ->
    (string -> Db.request_token -> Http.request -> Http.response) ->
    Http.response

  val access_resource :
    Http.request ->
    (string -> Db.access_token -> Http.request -> Http.response) ->
    Http.response

end
