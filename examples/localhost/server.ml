module Db =
struct

  module Http = Oauth_netcgi_http

  type consumer = string * string * Cryptokit.RSA.key
  let consumers = ["key", "secret", input_value (open_in "certificate.ocaml") ]
  let lookup_consumer k = List.find (fun (k',_,_) -> k' = k) consumers
  let consumer_key (k,_,_) = k
  let consumer_secret (_,s,_) = s
  let consumer_rsa_key (_,_,r) = r

  type request_token = consumer * string * string * bool ref
  let request_tokens = ref ([] : request_token list)
  let make_request_token c _ =
    let t = (c, Oauth_util.make_key (), Oauth_util.make_key (), ref false) in
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

module OS = Oauth_server.Make(Oauth_netcgi_http)(Db)

let authorize_get oauth_token request_token (cgi : Netcgi_types.cgi_activation) =
  Oauth_netcgi_http.respond cgi `Ok []
    (Printf.sprintf
        "
<html>
<head><title>Authorize</title></head>
<body>
  <form method=\"POST\" action=\"%s\">
    <input type=\"hidden\" name=\"oauth_token\" value=\"%s\">
    <input type=\"hidden\" name=\"oauth_callback\" value=\"%s\">
    <input type=\"submit\" value=\"Authorize\">
  </form>
</body>
</html>
"
        (cgi#url ())
        oauth_token
        (cgi#argument_value "oauth_callback"))

let authorize_post oauth_token request_token (cgi : Netcgi_types.cgi_activation) =
  let oauth_callback = cgi#argument_value "oauth_callback" in
  match oauth_callback with
    | "" -> Oauth_netcgi_http.respond cgi `Ok []
        "
<html>
<head><title>Authorized!</title></head>
<body>
  <p>Authorized!</p>
</body>
</html>
"
    | _ -> Oauth_netcgi_http.respond cgi `Found ["Location", oauth_callback] ""

let echo oauth_token access_token (cgi : Netcgi_types.cgi_activation) =
  Oauth_netcgi_http.respond cgi `Ok [] (Netencoding.Url.mk_url_encoded_parameters (Oauth_netcgi_http.arguments cgi))

let oauth_cgi_handler (cgi : Netcgi_types.cgi_activation) =
  let url = cgi#url ~with_authority:`None () in
  match Neturl.split_path url with
    | [ _; "request_token" ] -> OS.fetch_request_token cgi
    | [ _; "authorize" ]     -> OS.authorize_request_token cgi authorize_get authorize_post
    | [ _; "access_token" ]  -> OS.fetch_access_token cgi
    | [ _; "echo" ]          -> OS.access_resource cgi echo
    | _                      -> Oauth_netcgi_http.respond cgi `Not_found [] ""

let oauth_handler = {
  Nethttpd_services.dyn_handler = (fun _ -> oauth_cgi_handler);
  dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
  dyn_uri = None;
  dyn_translator = (fun _ -> "");
  dyn_accept_all_conditionals = false;
}

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let factories =
    [ Nethttpd_plex.nethttpd_factory ~handlers:[ "oauth", oauth_handler ] () ]
  in

  Netplex_main.startup
    (Netplex_mp.mp())
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    factories
    cmdline_cfg
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start()
;;
