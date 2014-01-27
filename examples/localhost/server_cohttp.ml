module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

module Db =
struct

  module Http = Oauth_cohttp_http

  module Client = struct
    type t = string * string
    let clients = ["key", "secret"]
    let find k = List.find (fun (k',_) -> k' = k) clients
    let id (k,_) = k
    let secret (_,s) = s
    let rsa_key _ = raise Not_found
  end

  module Temporary = struct
    type t = Client.t * string * string * bool ref
    let temporary_credentials = ref ([] : t list)
    let make c _ =
      let t = (c, Oauth_util.make_key (), Oauth_util.make_key (), ref true) in
      temporary_credentials := t::!temporary_credentials;
      t
    let find k = List.find (fun (_,k',_,_) -> k' = k) !temporary_credentials
    let check_client (c,_,_,_) c' = c = c'
    let key (_,k,_,_) = k
    let secret (_,_,s,_) = s
    let authorized (_,_,_,a) = !a
    let authorize (_,_,_,a) _ = a := true
  end

  module Token = struct
    type t = Client.t * string * string
    let token_credentials = ref []
    let exchange_temporary ((c,k,s,a) as rt) =
      if not !a
      then raise (Failure "access token not authorized");
      Temporary.(temporary_credentials := List.filter (fun rt' -> rt' <> rt) !temporary_credentials);
      let t = (c, Oauth_util.make_key (), Oauth_util.make_key ()) in
      token_credentials := t::!token_credentials;
      t
    let find k = List.find (fun (_,k',_) -> k' = k) !token_credentials
    let check_client (c,_,_) c' = c = c'
    let key (_,k,_) = k
    let secret (_,_,s) = s
  end

end

module OS = Oauth_server.Make(Oauth_cohttp_http)(Db)

let echo tok tok_secret req =
  CU.Server.respond_string ~status:`OK ~body:"Bleh!" ()

let oauth_callback conn_id ?body request =
  let open CU.Server in
  let uri = Request.uri request in
  let path = Uri.path uri in
  match path with
  | "/request_token" -> OS.fetch_temporary_credentials request
  | "/access_token" -> OS.fetch_token_credentials request
  | "/echo" -> OS.access_resource request echo
  | _ -> CU.Server.respond_not_found ()

let config = CU.Server.({
    callback=oauth_callback;
    conn_closed = (fun id () -> ())
})

let _ = Lwt_main.run
    (CU.Server.create ~address:"localhost" ~port:8787 config)
