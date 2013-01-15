let request
    ?(http_method = `Post)
    ~url
    ?(headers = [])
    ?(params = [])
    ?body
    () =
  let call =
    let make_url () =
      let query = Netencoding.Url.mk_url_encoded_parameters params in
      url ^ (if query <> "" then "?" ^ query else "")
    in
    match http_method, body with
      | `Post, None ->
          new Http_client.post url params
      | `Post, Some (content_type, body) ->
          let call = new Http_client.post_raw (make_url ()) body in
          (call#request_header `Base)#update_field "Content-type" content_type;
          call
      | `Delete, _ ->
          new Http_client.delete (make_url ())
      | `Put v, None ->
          new Http_client.put (make_url ()) v
      | `Put _, Some _ ->
          failwith "`Put error --> do not set body"
      | `Get, _ ->
          new Http_client.get (make_url ())
      | `Head, _ ->
          new Http_client.head (make_url ())
  in
  let h = call#request_header `Base in
  List.iter (fun (k,v) -> h#update_field k v) headers;

  let pipeline = new Http_client.pipeline in
  Ssl.init ();
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  let tct = (Https_client.https_transport_channel_type ctx :> Http_client.transport_channel_type) in
  pipeline#configure_transport Http_client.https_cb_id tct;

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
