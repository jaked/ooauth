(* works against the term.ie test server *)

module OC = Oauth_client.Make(Oauth_netclient_http_client)

let rsa_key = input_value (open_in "private_key.ocaml")
let oauth_signature_method = `Rsa_sha1 rsa_key
let http_method = `Post

let url s = "http://term.ie/oauth/example" ^ s

;;

let (oauth_token, oauth_token_secret) =
  OC.fetch_request_token
    ~http_method ~url:(url "/request_token.php")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    () in
prerr_endline ("oauth_token = " ^ oauth_token);
prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);

let (oauth_token, oauth_token_secret) =
  OC.fetch_access_token
    ~http_method ~url:(url "/access_token.php")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    ~oauth_token ~oauth_token_secret
      () in
prerr_endline ("oauth_token = " ^ oauth_token);
prerr_endline ("oauth_token_secret = " ^ oauth_token_secret);

let res =
  OC.access_resource
    ~http_method ~url:(url "/echo_api.php")
    ~oauth_signature_method
    ~oauth_consumer_key:"key" ~oauth_consumer_secret:"secret"
    ~oauth_token ~oauth_token_secret
    ~other_params:["method", "foo"; "bar", "baz"]
    () in
prerr_endline ("res = " ^ res);
