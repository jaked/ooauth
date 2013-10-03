open Oauth_common

let make_key ?(rng = Cryptokit.Random.device_rng "/dev/random") () =
  Cryptokit.Random.string rng 16 |>
      Cryptokit.transform_string (Oauth_base32.encode ())
