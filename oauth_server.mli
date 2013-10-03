module type Http =
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

  type request
  val http_method : request -> [ `GET | `POST | `HEAD ]
  val url : request -> string
  val header : request -> string -> string (* throws Not_found *)
  val argument : request -> ?default:string -> string -> string (* throws Not_found *)
  val arguments : request -> (string * string) list

  type response
  val respond : request -> status -> (string * string) list -> string -> response Monad.t

  exception Error of status * string
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

module Make (Http : Http) (Db : Db with module Http = Http) :
sig

  val fetch_request_token : Http.request -> Http.response Http.Monad.t

  val fetch_access_token : Http.request -> Http.response Http.Monad.t

  val authorize_request_token :
    Http.request ->
    (string -> Db.request_token -> Http.request -> Http.response Http.Monad.t) ->
    (string -> Db.request_token -> Http.request -> Http.response Http.Monad.t) ->
    Http.response Http.Monad.t

  val access_resource :
    Http.request ->
    (string -> Db.access_token -> Http.request -> Http.response Http.Monad.t) ->
    Http.response Http.Monad.t

end
