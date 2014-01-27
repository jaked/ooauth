module type Http =
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

  type request
  val http_method : request -> meth
  val url : request -> string
  val header : request -> string -> string (* throws Not_found *)
  val argument : request -> ?default:string -> string -> string (* throws Not_found *)
  val arguments : request -> (string * string) list

  type response
  val respond : request -> status_code -> (string * string) list -> string -> response Monad.t

  exception Error of status_code * string
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

module Make
  (Http : Http)
  (Db : DB with module Http = Http) =
struct

  let bad_request msg = raise (Http.Error (400, msg))
  let unauthorized msg = raise (Http.Error (401, msg))

  let with_oauth_params req f =
    let arg =
      try
        let h = Http.header req "Authorization" in
        let parts = Re_pcre.(split ~rex:(regexp "\\s*,\\s*") h) in
        let args =
          List.map
            (fun p ->
               match Re_pcre.(extract ~rex:(regexp "(\\S*)\\s*=\\s*\"([^\"]*)\"") p) with
               | [| _; k; v |] -> k, Oauth_common.rfc3986_decode v
               | _ -> raise Not_found) (* bad header, fall back to CGI args (?) *)
            parts in
        let arg ?default name =
          try List.assoc name args
          with Not_found as e ->
            match default with
            | Some d -> d
            | _ -> raise e in
        arg
      with Not_found -> Http.argument req in

    let required_arg name =
      try arg name
      with Not_found -> bad_request ("missing parameter " ^ name) in
    let optional_arg name =
      try Some (arg name)
      with Not_found -> None in

    let http_method = Http.http_method req in
    let url = Http.url req in

    let oauth_client           = required_arg "oauth_consumer_key" in
    let oauth_token            = optional_arg "oauth_token" in
    let oauth_signature_method = required_arg "oauth_signature_method" in
    let oauth_signature        = required_arg "oauth_signature" in
    let oauth_timestamp        = required_arg "oauth_timestamp" in
    let oauth_nonce            = required_arg "oauth_nonce" in
    let oauth_version          = arg ~default:"1.0" "oauth_version" in

    if oauth_version <> "1.0" then bad_request ("unsupported version " ^ oauth_version);

    let client_credentials =
      try Db.Client.find oauth_client
      with Not_found -> unauthorized "invalid client" in
    let oauth_client_secret = Db.Client.secret client_credentials in
    let oauth_signature_method =
      try
        Oauth_common.signature_method_of_string
          (fun () ->
            try Db.Client.rsa_key client_credentials
            with Not_found -> unauthorized "no RSA key")
          oauth_signature_method
      with Not_found ->
        bad_request ("unsupported signature method " ^ oauth_signature_method) in
    let oauth_timestamp =
      try float_of_string (oauth_timestamp ^ ".")
      with Failure _ -> 0. in

    f
      ~http_method ~url ~client_credentials
      ~oauth_client ~oauth_client_secret
      ~oauth_signature_method ~oauth_signature
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?oauth_token
      ()



  let fetch_temporary_credentials req =
    let frt
        ~http_method ~url ~client_credentials
        ~oauth_client ~oauth_client_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_client ~oauth_client_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then
        let request_token = Db.Temporary.make client_credentials req in
        Http.respond req 200 []
          (Uri.encoded_of_query [
            "oauth_token", [Db.Temporary.key request_token];
            "oauth_token_secret", [Db.Temporary.secret request_token];
          ])
      else unauthorized "invalid signature" in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg



  let fetch_token_credentials req =
    let frt
        ~http_method ~url ~client_credentials
        ~oauth_client ~oauth_client_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      let request_token =
        match oauth_token with
          | None -> bad_request "missing parameter oauth_token"
          | Some t ->
              try Db.Temporary.find t
              with Not_found -> unauthorized "invalid request token" in
      if not (Db.Temporary.check_client request_token client_credentials)
      then bad_request "client/temporary token mismatch";
      let oauth_token = Db.Temporary.key request_token in
      let oauth_token_secret = Db.Temporary.secret request_token in
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_client ~oauth_client_secret
          ~oauth_token ~oauth_token_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then
        let access_token =
          try Db.Token.exchange_temporary request_token
          with Failure msg -> unauthorized msg in
        Http.respond req 200 []
          (Uri.encoded_of_query [
            "oauth_token", [Db.Token.key access_token];
            "oauth_token_secret", [Db.Token.secret access_token];
          ])
      else unauthorized "invalid signature" in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg



  let authorize_temporary_credentials req kget kpost =
    try
      let oauth_token =
        try Http.argument req "oauth_token"
        with Not_found -> bad_request "missing parameter oauth_token" in
      let request_token =
        try Db.Temporary.find oauth_token
        with Not_found -> unauthorized "invalid request token" in
      if Db.Temporary.authorized request_token
      then bad_request "request token already authorized";

      match Http.http_method req with
        | `GET ->
            kget oauth_token request_token req
        | `POST ->
            Db.Temporary.authorize request_token req;
            kpost oauth_token request_token req
        | _ -> raise (Http.Error (405, ""))

    with Http.Error (status, msg) -> Http.respond req status [] msg



  let access_resource req k =
    let frt
        ~http_method ~url ~client_credentials
        ~oauth_client ~oauth_client_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      let access_token =
        match oauth_token with
          | None -> bad_request "missing parameter oauth_token"
          | Some t ->
              try Db.Token.find t
              with Not_found -> unauthorized "invalid access token" in
      if not (Db.Token.check_client access_token client_credentials)
      then bad_request "client/token mismatch";
      let oauth_token = Db.Token.key access_token in
      let oauth_token_secret = Db.Token.secret access_token in
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_client ~oauth_client_secret
          ~oauth_token ~oauth_token_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then k oauth_token access_token req
      else unauthorized "invalid signature"in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg

end
