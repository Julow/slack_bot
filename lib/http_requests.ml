open Yojson.Safe
open Yojson.Safe.Util

type t = { token : string }

let v ~token () = { token }

open Lwt.Syntax

let ( >>=@ ) = Lwt_result.bind

(** Does a POST request when [body] is passed, a GET otherwise. *)
let make_json_request ~uri ?body t =
  let uri = Uri.of_string uri in
  let headers extra_headers =
    Cohttp.Header.of_list
      (("Authorization", "Bearer " ^ t.token) :: extra_headers)
  in
  let* rsp, body =
    match body with
    | Some body ->
        let serialized_body = Yojson.Basic.to_string body in
        let headers = headers [ ("Content-type", "application/json") ] in
        Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri
    | None ->
        let headers = headers [] in
        Cohttp_lwt_unix.Client.get ~headers uri
  in
  let+ body = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.(code_of_status rsp.status |> is_success) then Ok body
  else Error body

let write_matches ~channel output t =
  let body =
    `Assoc [ ("channel", `String channel); ("text", `String output) ]
  in
  make_json_request ~uri:"https://slack.com/api/chat.postMessage" ~body t
  >>=@ fun body ->
  try Lwt.return_ok (from_string body)
  with Yojson.Json_error err -> Lwt.return_error err

let parse_reactions_response resp =
  try
    Ok
      (List.sort_uniq String.compare
         (List.map Util.to_string
            (List.map
               Util.(member "users")
               (from_string resp
               |> Util.(member "message")
               |> Util.(member "reactions")
               |> Util.to_list)
            |> Util.flatten)))
  with Yojson.Json_error err -> Error err

let get_reactions ~channel ~timestamp t =
  let uri =
    Format.sprintf "https://slack.com/api/reactions.get?channel=%s&timestamp=%s"
      channel timestamp
  in
  make_json_request ~uri t >>=@ fun resp ->
  Lwt.return (parse_reactions_response resp)

let parse_ts resp = from_string resp |> member "ts" |> to_string

let write_opt_in_message ~channel t =
  let message =
    "Hi <!here>, who wants to pair-program this week? To opt in, react to this \
     message, for example with a :raised_hand::skin-tone-4:"
  in
  let body =
    `Assoc [ ("channel", `String channel); ("text", `String message) ]
  in
  make_json_request ~uri:"https://slack.com/api/chat.postMessage" ~body t
  >>=@ fun body ->
  try Lwt.return_ok (parse_ts body)
  with Yojson.Json_error err -> Lwt.return_error err
