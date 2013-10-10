(* works against the term.ie test server *)

module OC = Oauth_client.Make(Oauth_cohttp_http_client)

let (>>=) = Lwt.bind

(* from http://term.ie/oauth/example *)
let oauth_client = "key"
let oauth_client_secret = "secret"

let oauth_signature_method = `Hmac_sha1
let http_method = `POST
let url s = "http://term.ie/oauth/example" ^ s

let fetch_temporary_credentials () =
    OC.fetch_temporary_credentials
      ~http_method
      ~url:(url "/request_token.php")
      ~oauth_signature_method
      ~oauth_client
      ~oauth_client_secret
      () >>= fun (oauth_token, oauth_token_secret) ->
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  Lwt.return (oauth_token, oauth_token_secret)

let fetch_token_credentials oauth_token oauth_token_secret =
    OC.fetch_token_credentials
      ~http_method
      ~url:(url "/access_token.php")
      ~oauth_signature_method
      ~oauth_client
      ~oauth_client_secret
      ~oauth_token
      ~oauth_token_secret
      () >>= fun (oauth_token, oauth_token_secret) ->
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  Lwt.return (oauth_token, oauth_token_secret)

let access_resource oauth_token oauth_token_secret =
    OC.access_resource
      ~http_method ~url:(url "/echo_api.php")
      ~oauth_signature_method
      ~oauth_client
      ~oauth_client_secret
      ~oauth_token
      ~oauth_token_secret
      ~params:["method", "foo"; "bar", "baz"]
      () >>= fun res ->
    Lwt_io.eprintl ("res = " ^ res)

let _ = Lwt_main.run
    (fetch_temporary_credentials () >>= fun (t, st) ->
     fetch_token_credentials t st >>= fun (t, st) ->
     access_resource t st)
