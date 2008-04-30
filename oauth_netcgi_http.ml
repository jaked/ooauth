exception Error of Nethttp.http_status * string

type request = Netcgi_types.cgi_activation

let http_method (cgi : Netcgi_types.cgi_activation) =
  match cgi#request_method with
    | `GET -> `Get
    | `HEAD -> `Head
    | `POST -> `Post
    | `DELETE | `PUT _ -> raise (Error (`Method_not_allowed, ""))

let url (cgi : Netcgi_types.cgi_activation) =
  cgi#url ()

let header (cgi : Netcgi_types.cgi_activation) h =
  cgi#environment#input_header_field h

let argument (cgi : Netcgi_types.cgi_activation) ?default a =
  try (cgi#argument a)#value
  with Not_found as e ->
    match default with
      | Some d -> d
      | None -> raise e

let arguments (cgi : Netcgi_types.cgi_activation) =
  List.map (fun (k,v) -> k, v#value) cgi#arguments

type response = unit

let respond (cgi : Netcgi_types.cgi_activation) status fields body =
  let fields = List.map (fun (k, v) -> k, [v]) fields in
  cgi#set_header ~status ~fields ();
  cgi#output#output_string body;
  cgi#output#commit_work ()
