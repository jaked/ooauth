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

module Make
  (Http : Http)
  (Db : Db with module Http = Http) =
struct

  let bad_request msg = raise (Http.Error (`Bad_request, msg))
  let unauthorized msg = raise (Http.Error (`Unauthorized, msg))

  let with_oauth_params req f =
    let arg =
      try
        let h = Http.header req "Authorization" in
        let parts = Pcre.split ~pat:"\\s*,\\s*" h in
        let args =
          List.map
            (fun p ->
              match Pcre.extract ~pat:"(\\S*)\\s*=\\s*\"([^\"]*)\"" p with
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

    let oauth_consumer_key     = required_arg "oauth_consumer_key" in
    let oauth_token            = optional_arg "oauth_token" in
    let oauth_signature_method = required_arg "oauth_signature_method" in
    let oauth_signature        = required_arg "oauth_signature" in
    let oauth_timestamp        = required_arg "oauth_timestamp" in
    let oauth_nonce            = required_arg "oauth_nonce" in
    let oauth_version          = arg ~default:"1.0" "oauth_version" in

    if oauth_version <> "1.0" then bad_request ("unsupported version " ^ oauth_version);

    let consumer =
      try Db.lookup_consumer oauth_consumer_key
      with Not_found -> unauthorized "invalid consumer key" in
    let oauth_consumer_secret = Db.consumer_secret consumer in
    let oauth_signature_method =
      try
        Oauth_common.signature_method_of_string
          (fun () ->
            try Db.consumer_rsa_key consumer
            with Not_found -> unauthorized "no RSA key")
          oauth_signature_method
      with Not_found ->
        bad_request ("unsupported signature method " ^ oauth_signature_method) in
    let oauth_timestamp =
      try float_of_string (oauth_timestamp ^ ".")
      with Failure _ -> 0. in

    f
      ~http_method ~url ~consumer
      ~oauth_consumer_key ~oauth_consumer_secret
      ~oauth_signature_method ~oauth_signature
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?oauth_token
      ()



  let fetch_request_token req =
    let frt
        ~http_method ~url ~consumer
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_consumer_key ~oauth_consumer_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then
        let request_token = Db.make_request_token consumer req in
        Http.respond req `Ok []
          (Netencoding.Url.mk_url_encoded_parameters [
            "oauth_token", Db.request_token_token request_token;
            "oauth_token_secret", Db.request_token_secret request_token;
          ])
      else unauthorized "invalid signature" in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg



  let fetch_access_token req =
    let frt
        ~http_method ~url ~consumer
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      let request_token =
        match oauth_token with
          | None -> bad_request "missing parameter oauth_token"
          | Some t ->
              try Db.lookup_request_token t
              with Not_found -> unauthorized "invalid request token" in
      if not (Db.request_token_check_consumer request_token consumer)
      then bad_request "consumer/request token mismatch";
      let oauth_token = Db.request_token_token request_token in
      let oauth_token_secret = Db.request_token_secret request_token in
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_consumer_key ~oauth_consumer_secret
          ~oauth_token ~oauth_token_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then
        let access_token =
          try Db.exchange_request_token request_token
          with Failure msg -> unauthorized msg in
        Http.respond req `Ok []
          (Netencoding.Url.mk_url_encoded_parameters [
            "oauth_token", Db.access_token_token access_token;
            "oauth_token_secret", Db.access_token_secret access_token;
          ])
      else unauthorized "invalid signature" in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg



  let authorize_request_token req kget kpost =
    try
      let oauth_token =
        try Http.argument req "oauth_token"
        with Not_found -> bad_request "missing parameter oauth_token" in
      let request_token =
        try Db.lookup_request_token oauth_token
        with Not_found -> unauthorized "invalid request token" in
      if Db.request_token_authorized request_token
      then bad_request "request token already authorized";

      match Http.http_method req with
        | `Get ->
            kget oauth_token request_token req
        | `Post ->
            Db.authorize_request_token request_token req;
            kpost oauth_token request_token req
        | _ -> raise (Http.Error (`Method_not_allowed, ""))

    with Http.Error (status, msg) -> Http.respond req status [] msg



  let access_resource req k =
    let frt
        ~http_method ~url ~consumer
        ~oauth_consumer_key ~oauth_consumer_secret
        ~oauth_signature_method ~oauth_signature
        ~oauth_timestamp ~oauth_nonce ~oauth_version
        ?oauth_token
        () =
      let access_token =
        match oauth_token with
          | None -> bad_request "missing parameter oauth_token"
          | Some t ->
              try Db.lookup_access_token t
              with Not_found -> unauthorized "invalid access token" in
      if not (Db.access_token_check_consumer access_token consumer)
      then bad_request "consumer/access token mismatch";
      let oauth_token = Db.access_token_token access_token in
      let oauth_token_secret = Db.access_token_secret access_token in
      if
        Oauth_common.check_signature
          ~http_method ~url
          ~oauth_signature_method ~oauth_signature
          ~oauth_consumer_key ~oauth_consumer_secret
          ~oauth_token ~oauth_token_secret
          ~oauth_timestamp ~oauth_nonce ~oauth_version
          ~params:(Http.arguments req)
          ()
      then k oauth_token access_token req
      else unauthorized "invalid signature"in

    try with_oauth_params req frt
    with Http.Error (status, msg) -> Http.respond req status [] msg

end
