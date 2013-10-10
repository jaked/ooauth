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

  type request
  val http_method : request -> meth
  val url : request -> string
  val header : request -> string -> string (* throws Not_found *)
  val argument : request -> ?default:string -> string -> string (* throws Not_found *)
  val arguments : request -> (string * string) list

  type response
  val respond : request -> status -> (string * string) list -> string -> response Monad.t

  exception Error of status * string
end

module type DB =
sig
  module Http : Http

  module Client : sig
    type t
    val find : string -> t (* throws Not_found *)
    val id : t -> string
    val secret : t -> string
    val rsa_key : t -> Cryptokit.RSA.key (* throws Not_found *)
  end

  module Temporary : sig
    type t
    val temporary_credentials : t list ref
    val make : Client.t -> Http.request -> t
    val find : string -> t (* throws Not_found *)
    val check_client : t -> Client.t -> bool
    val key : t -> string
    val secret : t -> string
    val authorized : t -> bool
    val authorize : t -> Http.request -> unit (* throws Failure *)
  end

  module Token : sig
    type t
    val exchange_temporary : Temporary.t -> t (* throws Failure *)
    val find : string -> t (* throws Not_found *)
    val check_client : t -> Client.t -> bool
    val key : t -> string
    val secret : t -> string
    end
end

module Make (Http : Http) (Db : DB with module Http = Http) :
sig

  val fetch_temporary_credentials : Http.request -> Http.response Http.Monad.t

  val fetch_token_credentials : Http.request -> Http.response Http.Monad.t

  val authorize_temporary_credentials :
    Http.request ->
    (string -> Db.Temporary.t -> Http.request -> Http.response Http.Monad.t) ->
    (string -> Db.Temporary.t -> Http.request -> Http.response Http.Monad.t) ->
    Http.response Http.Monad.t

  val access_resource :
    Http.request ->
    (string -> Db.Token.t -> Http.request -> Http.response Http.Monad.t) ->
    Http.response Http.Monad.t

end
