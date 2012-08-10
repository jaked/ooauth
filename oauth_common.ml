let (|>) x f = f x (* so function pipelines read left to right *)

let opt_param name param =
  match param with
    | None -> []
    | Some p -> [name, p]

let rng = Cryptokit.Random.device_rng "/dev/random"

let rfc3986_encode s = Netencoding.Url.encode ~plus:false s
let rfc3986_decode s = Netencoding.Url.decode s

let string_of_http_method = function
  | `Get -> "GET"
  | `Post -> "POST"
  | `Head -> "HEAD"
  | `Delete -> "DELETE"
  | `Put _ -> "PUT"

let string_of_signature_method = function
  | `Plaintext -> "PLAINTEXT"
  | `Hmac_sha1 -> "HMAC-SHA1"
  | `Rsa_sha1 _ -> "RSA-SHA1"

let signature_method_of_string rsa_key = function
  | "PLAINTEXT" -> `Plaintext
  | "HMAC-SHA1" -> `Hmac_sha1
  | "RSA-SHA1"  -> `Rsa_sha1 (rsa_key ())
  | _ -> raise Not_found

let normalize_url url =
  let url = Neturl.parse_url ~enable_fragment:true url in
  let url = Neturl.remove_from_url ~query:true ~fragment:true url in
  Neturl.string_of_url url

let string_of_timestamp t =
  let s = string_of_float t in
  String.sub s 0 (String.length s - 1)

let make_timestamp () = Unix.time ()

let make_nonce () =
  Cryptokit.Random.string rng 16 |>
      Cryptokit.transform_string (Cryptokit.Hexa.encode ())

let base64_encode v =
  let b64 = Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) v in
  b64 ^ "="

let base64_decode v =
  Cryptokit.transform_string (Cryptokit.Base64.decode ()) v

let hmac_sha1_hash text key =
  text |>
      Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 key) |>
          base64_encode

let sha1_digest_info h =
  "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14" ^ h

let pkcs1_pad rsa_key v =
  let tLen = String.length v in
  let emLen = rsa_key.Cryptokit.RSA.size / 8 in
  "\x00\x01" ^ String.make (emLen - tLen - 3) '\xff' ^ "\x00" ^ v

let rsa_sha1_hash text rsa_key =
  text |>
      Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) |>
          sha1_digest_info |>
              pkcs1_pad rsa_key |>
                  Cryptokit.RSA.sign rsa_key |>
                      base64_encode

let check_rsa_sha1_hash text rsa_key signature =
  try
    (text |>
        Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) |>
            sha1_digest_info |>
                pkcs1_pad rsa_key) =
    (signature |>
        base64_decode |>
            Cryptokit.RSA.unwrap_signature rsa_key)
  with _ -> false



let signature_base_string
    ~http_method ~url
    ~oauth_signature_method
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?(params = [])
    () =

  let params = [
    "oauth_signature_method", string_of_signature_method oauth_signature_method;
    "oauth_consumer_key", oauth_consumer_key;
    "oauth_timestamp", string_of_timestamp oauth_timestamp;
    "oauth_nonce", oauth_nonce;
      "oauth_version", oauth_version;
  ] @
    opt_param "oauth_token" oauth_token @
    List.filter (fun (k, v) -> k <> "oauth_signature") params in

  List.map rfc3986_encode
    [
      string_of_http_method http_method;
      normalize_url url;

      params |>
          List.map (fun (k, v) -> rfc3986_encode k, rfc3986_encode v) |>
              List.sort (fun (k,v) (k',v') ->
                match String.compare k k' with
                  | 0 -> String.compare v v'
                  | c -> c) |>
                  List.map (fun (k,v) -> k ^ "=" ^ v) |>
                      String.concat "&"
    ] |> String.concat "&"



let sign
    ~http_method ~url
    ~oauth_signature_method
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?params
    () =

  let key =
    (rfc3986_encode oauth_consumer_secret ^ "&" ^
        match oauth_token_secret with
          | None -> ""
          | Some s -> rfc3986_encode s) in

  let signature_base_string =
    signature_base_string
      ~http_method ~url
      ~oauth_signature_method
      ~oauth_consumer_key ~oauth_consumer_secret
      ?oauth_token ?oauth_token_secret
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?params
      () in

  match oauth_signature_method with
    | `Plaintext -> rfc3986_encode key
    | `Hmac_sha1 -> hmac_sha1_hash signature_base_string key
    | `Rsa_sha1 rsa_key -> rsa_sha1_hash signature_base_string rsa_key



let check_signature
    ~http_method ~url
    ~oauth_signature_method ~oauth_signature
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?params
    () =

  let key =
    (rfc3986_encode oauth_consumer_secret ^ "&" ^
        match oauth_token_secret with
          | None -> ""
          | Some s -> rfc3986_encode s) in

  let signature_base_string =
    signature_base_string
      ~http_method ~url
      ~oauth_signature_method
      ~oauth_consumer_key ~oauth_consumer_secret
      ?oauth_token ?oauth_token_secret
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?params
      () in

  match oauth_signature_method with
    | `Plaintext -> rfc3986_encode key = oauth_signature
    | `Hmac_sha1 -> hmac_sha1_hash signature_base_string key = oauth_signature
    | `Rsa_sha1 rsa_key -> check_rsa_sha1_hash signature_base_string rsa_key oauth_signature
