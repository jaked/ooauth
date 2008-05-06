let request
    ?(http_method = `Post)
    ~url
    ?(headers = [])
    ?(params = [])
    ?body
    () =
  let call =
    match http_method, body with
      | `Post, None ->
          new Http_client.post url params
      | `Post, Some (content_type, body) ->
          let query = Netencoding.Url.mk_url_encoded_parameters params in
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          let call = new Http_client.post_raw url body in
          (call#request_header `Base)#update_field "Content-type" content_type;
          call
      | `Get, _ | `Head, _ ->
          let query = Netencoding.Url.mk_url_encoded_parameters params in
          let url = url ^ (if query <> "" then "?" ^ query else "") in
          match http_method with
            | `Get  -> new Http_client.get url
            | `Head -> new Http_client.head url
            | `Post -> assert false in

  let h = call#request_header `Base in
  List.iter (fun (k,v) -> h#update_field k v) headers;

  let pipeline = new Http_client.pipeline in
(*
  pipeline#set_proxy "localhost" 9888;
  let url = Neturl.parse_url url in
  let host = Neturl.url_host url in
  let port = try Neturl.url_port url with Not_found -> 80 in
  let url = if port = 80 then host else host ^ ":" ^ string_of_int port in
  h#update_field "Host" url;
*)
  pipeline#add call;
  pipeline#run();

  (call#response_status, call#response_header#fields, call#response_body#value)
