(* works against the term.ie test server *)

module OC = Oauth_client.Make(Oauth_netclient_http_client)

let rsa_key = Rsa.read_rsa_privkey "private_key.pem"

(* from http://term.ie/oauth/example *)
let oauth_consumer_key = "key"
let oauth_consumer_secret = "secret"

let oauth_signature_method = `Rsa_sha1 rsa_key
let http_method = `Post
let url s = "http://term.ie/oauth/example" ^ s

let fetch_request_token () =
  let oauth_token, oauth_token_secret =
    OC.fetch_request_token
      ~http_method
      ~url:(url "/request_token.php")
      ~oauth_signature_method
      ~oauth_consumer_key
      ~oauth_consumer_secret
      () in
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  oauth_token, oauth_token_secret

let fetch_access_token oauth_token oauth_token_secret =
  let oauth_token, oauth_token_secret =
    OC.fetch_access_token
      ~http_method
      ~url:(url "/access_token.php")
      ~oauth_signature_method
      ~oauth_consumer_key
      ~oauth_consumer_secret
      ~oauth_token
      ~oauth_token_secret
      () in
  prerr_endline ("oauth_token = " ^ oauth_token);
  prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);
  oauth_token, oauth_token_secret

let access_resource oauth_token oauth_token_secret =
  let res =
    OC.access_resource
      ~http_method ~url:(url "/echo_api.php")
      ~oauth_signature_method
      ~oauth_consumer_key
      ~oauth_consumer_secret
      ~oauth_token
      ~oauth_token_secret
      ~params:["method", "foo"; "bar", "baz"]
      () in
  prerr_endline ("res = " ^ res)

let _ =
  let t, st = fetch_request_token () in
  let t, st = fetch_access_token t st in
  access_resource t st
