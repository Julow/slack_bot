open Slack_bot_lib
open Yojson.Basic
open Yojson.Basic.Util

let config =
  let config_file =
    try Sys.argv.(1) with _ -> failwith "Missing argument: config."
  in
  from_file config_file

(* let test_channel = member "test_channel_id" config |> to_string *)
let real_channel = member "adoption_channel_id" config |> to_string

let token = member "token" config |> to_string
(* let bot_id = member "bot_id" config |> to_string *)

open Types

(* let test_case = *)
(*   { channel = test_channel; db_path = "irmin/pairing_bot_testing"; num_iter = 1000 } *)

let real_case =
  {
    channel = real_channel;
    db_path = "irmin/pairing_bot";
    num_iter = 100000000;
  }

let write_matches_to_irmin_and_slack ~http our_match case =
  let open Lwt.Syntax in
  let output = Match.to_string our_match in
  let () = Printf.printf "%s" output in
  let* result = Http_requests.write_matches ~channel:case.channel output http in
  match result with
  | Ok _ -> Irmin_io.write_matches_to_irmin our_match case.db_path
  | Error e ->
      Format.printf "Http Request to write to slack failed with error : %s" e;
      Lwt.return ()

let write_opt_in_to_irmin_and_slack ~http case =
  let open Lwt.Syntax in
  let* result = Http_requests.write_opt_in_message ~channel:case.channel http in
  match result with
  | Ok ts -> Irmin_io.write_timestamp_to_irmin ts case.db_path
  | Error e ->
      Format.printf "Http Request to write to slack failed with error : %s" e;
      Lwt.return ()

let main case =
  let open Lwt.Syntax in
  let http = Http_requests.v ~token () in
  let* () = Schedule.sleep_till `Thu (11, 0, 0) in
  let* () = write_opt_in_to_irmin_and_slack ~http case in
  let* () = Schedule.sleep_till `Fri (11, 0, 0) in
  let* timestamp = Irmin_io.read_timestamp_from_irmin case.db_path in
  let* members =
    let+ m =
      Http_requests.get_reactions ~channel:case.channel ~timestamp http
    in
    match m with Error _ -> assert false | Ok m -> m
  in
  let* most_optimum = Match.get_most_optimum case members in
  write_matches_to_irmin_and_slack ~http most_optimum case

let () =
  while true do
    Lwt_main.run (main real_case)
  done
