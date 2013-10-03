exception Error of Nethttp.http_status * string

type request = Netcgi.cgi_activation

let http_method (cgi : Netcgi.cgi_activation) =
  match cgi#request_method with
    | `GET -> `Get
    | `HEAD -> `Head
    | `POST -> `Post
    | `DELETE | `PUT _ -> raise (Error (`Method_not_allowed, ""))

let url (cgi : Netcgi.cgi_activation) =
  cgi#url ()

let header (cgi : Netcgi.cgi_activation) h =
  cgi#environment#input_header_field h

let argument (cgi : Netcgi.cgi_activation) ?default a =
  try
    match cgi#environment#input_content_type_string with
      | "application/x-www-form-urlencoded" -> (cgi#argument a)#value
      | _ -> List.assoc a (Netencoding.Url.dest_url_encoded_parameters (cgi#environment#cgi_query_string))
  with Not_found as e ->
    match default with
      | Some d -> d
      | None -> raise e

let arguments (cgi : Netcgi.cgi_activation) =
  match cgi#environment#input_content_type_string with
    | "application/x-www-form-urlencoded" -> List.map (fun a -> a#name, a#value) cgi#arguments
    | _ -> Netencoding.Url.dest_url_encoded_parameters (cgi#environment#cgi_query_string)

type response = unit

let respond (cgi : Netcgi.cgi_activation) status fields body =
  let fields = List.map (fun (k, v) -> k, [v]) fields in
  cgi#set_header ~status ~fields ();
  cgi#output#output_string body;
  cgi#output#commit_work ()
