(* works against local test server *)

(*
  unfortunately Oauth_ocurl_http_client does not work with the test
  server (checked with curl 7.18.1 and Ocamlnet 2.2.9) because of a
  problem with the 100 Continue status line--Ocurl returns the 100
  status instead of the real status. not sure who is at fault.
*)
module OC = Oauth_client.Make(Oauth_cohttp_http_client)

let (>>=) = Lwt.bind

let oauth_signature_method = `Hmac_sha1
let http_method = `POST

let url s = "http://localhost:8767" ^ s

let fetch_request_token () =
  OC.fetch_request_token
    ~http_method ~url:(url "/request_token")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
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

let fetch_access_token oauth_token oauth_token_secret =
  OC.fetch_access_token
    ~http_method ~url:(url "/access_token")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
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
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    ~oauth_token ~oauth_token_secret
    ~params:["method", "foo"; "bar", "baz"]
    () >>= fun res ->
  prerr_endline ("res = " ^ res); Lwt.return ()

let _ = Lwt_main.run
          (
            fetch_request_token () >>= fun (t, ts) ->
            authorize t >>= fun () ->
            fetch_access_token t ts >>= fun (t, ts) ->
            access_resource t ts
          )
