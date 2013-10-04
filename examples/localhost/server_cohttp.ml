module C = Cohttp
module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

module Db =
struct

  module Http = Oauth_cohttp_http

  type consumer = string * string
  let consumers = ["key", "secret"]
  let lookup_consumer k = List.find (fun (k',_) -> k' = k) consumers
  let consumer_key (k,_) = k
  let consumer_secret (_,s) = s
  let consumer_rsa_key _ = raise Not_found

  type request_token = consumer * string * string * bool ref
  let request_tokens = ref ([] : request_token list)
  let make_request_token c _ =
    let t = (c, Oauth_util.make_key (), Oauth_util.make_key (), ref true) in
    request_tokens := t::!request_tokens;
    t
  let lookup_request_token k = List.find (fun (_,k',_,_) -> k' = k) !request_tokens
  let request_token_check_consumer (c,_,_,_) c' = c = c'
  let request_token_token (_,k,_,_) = k
  let request_token_secret (_,_,s,_) = s
  let request_token_authorized (_,_,_,a) = !a
  let authorize_request_token (_,_,_,a) _ = a := true

  type access_token = consumer * string * string
  let access_tokens = ref []
  let exchange_request_token ((c,k,s,a) as rt) =
    if not !a
    then raise (Failure "access token not authorized");
    request_tokens := List.filter (fun rt' -> rt' <> rt) !request_tokens;
    let t = (c, Oauth_util.make_key (), Oauth_util.make_key ()) in
    access_tokens := t::!access_tokens;
    t
  let lookup_access_token k = List.find (fun (_,k',_) -> k' = k) !access_tokens
  let access_token_check_consumer (c,_,_) c' = c = c'
  let access_token_token (_,k,_) = k
  let access_token_secret (_,_,s) = s

end

module OS = Oauth_server.Make(Oauth_cohttp_http)(Db)

let echo tok tok_secret req =
  CU.Server.respond_string ~status:`OK ~body:"Bleh!" ()

let oauth_callback conn_id ?body request =
  let open CU.Server in
  let uri = Request.uri request in
  let path = Uri.path uri in
  match path with
  | "/request_token" -> OS.fetch_request_token request
  | "/access_token" -> OS.fetch_access_token request
  | "/echo" -> OS.access_resource request echo
  | _ -> CU.Server.respond_not_found ()

let config = CU.Server.({
    callback=oauth_callback;
    conn_closed = (fun id () -> ())
})

let _ = Lwt_main.run
    (CU.Server.create ~address:"localhost" ~port:8787 config)
