(* works against local test server *)

(*
  unfortunately Oauth_ocurl_http_client does not work with the test
  server (checked with curl 7.18.1 and Ocamlnet 2.2.9) because of a
  problem with the 100 Continue status line--Ocurl returns the 100
  status instead of the real status. not sure who is at fault.
*)
module OC = Oauth_client.Make(Oauth_cohttp_http_client)

let (>>=) = Lwt.bind

let oauth_client = "key"
let oauth_client_secret = "secret"
let oauth_signature_method = `Hmac_sha1
let http_method = `POST

let url s = "http://localhost:8787" ^ s

let fetch_temporary_credentials () =
  OC.fetch_temporary_credentials
    ~http_method ~url:(url "/request_token")
    ~oauth_signature_method
    ~oauth_client ~oauth_client_secret
    ()
  >>= fun (oauth_token, oauth_token_secret) ->
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  Lwt.return (oauth_token, oauth_token_secret)

let authorize oauth_token =
  Oauth_cohttp_http_client.request
    ~http_method:`POST
    ~url:(url "/authorize")
    ~params:["oauth_token", oauth_token]
    () >>= fun _ -> Lwt.return ()

let fetch_token_credentials oauth_token oauth_token_secret =
  OC.fetch_token_credentials
    ~http_method ~url:(url "/access_token")
    ~oauth_signature_method
    ~oauth_client ~oauth_client_secret
    ~oauth_token ~oauth_token_secret
    ()
  >>= fun (oauth_token, oauth_token_secret) ->
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  Lwt.return (oauth_token, oauth_token_secret)

let access_resource oauth_token oauth_token_secret =
  OC.access_resource
    ~http_method ~url:(url "/echo")
    ~oauth_signature_method
    ~oauth_client ~oauth_client_secret
    ~oauth_token ~oauth_token_secret
    ~params:["method", "foo"; "bar", "baz"]
    () >>= fun res ->
  prerr_endline ("res = " ^ res); Lwt.return ()

let _ = Lwt_main.run
          (
            fetch_temporary_credentials () >>= fun (t, ts) ->
            (* authorize t >>= fun () -> *)
            fetch_token_credentials t ts >>= fun (t, ts) ->
            access_resource t ts
          )
