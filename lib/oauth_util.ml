open Oauth_common

let make_key ?(rng = Cryptokit.Random.device_rng "/dev/urandom") () =
  Cryptokit.Random.string rng 16 |>
      Cryptokit.transform_string (Oauth_base32.encode ())
