(* works against local test server *)

(*
  unfortunately Oauth_ocurl_http_client does not work with the test
  server (checked with curl 7.18.1 and Ocamlnet 2.2.9) because of a
  problem with the 100 Continue status line--Ocurl returns the 100
  status instead of the real status. not sure who is at fault.
*)
module OC = Oauth_client.Make(Oauth_netclient_http_client)

let rsa_key = input_value (open_in "private_key.ocaml")
let oauth_signature_method = `Rsa_sha1 rsa_key
let http_method = `Post

let url s = "http://localhost:8767" ^ s

;;

let (oauth_token, oauth_token_secret) =
  OC.fetch_request_token
    ~http_method ~url:(url "/request_token")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    () in
prerr_endline ("oauth_token = " ^ oauth_token);
prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);

ignore(Oauth_netclient_http_client.request
          `Post
          (url "/authorize")
          []
          ["oauth_token", oauth_token]);

let (oauth_token, oauth_token_secret) =
  OC.fetch_access_token
    ~http_method ~url:(url "/access_token")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    ~oauth_token ~oauth_token_secret
    () in
prerr_endline ("oauth_token = " ^ oauth_token);
prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);

let res =
  OC.access_resource
    ~http_method ~url:(url "/echo")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    ~oauth_token ~oauth_token_secret
    ~other_params:["method", "foo"; "bar", "baz"]
    () in
prerr_endline ("res = " ^ res);
