open Slack_bot_lib

module Test_schedule = struct
  let time_since_last_message ~name ~now ~schedule_day ~schedule_time ~expected
      =
    let f () =
      let result = Schedule.secs_till ~now schedule_day schedule_time in
      Alcotest.(check int) "should be equal to" expected result
    in
    (name, `Quick, f)

  let tests =
    (* Mon Jan 3 10:00:00 AM UTC 2022 *)
    let now = Option.get (Ptime.of_float_s 1641204000.0) in
    [
      time_since_last_message ~name:"Scheduled tomorrow" ~now ~schedule_day:`Tue
        ~schedule_time:(10, 0, 0) ~expected:86400
      (* 24h *);
      time_since_last_message ~name:"Scheduled today" ~now ~schedule_day:`Mon
        ~schedule_time:(11, 0, 0) ~expected:3600
      (* An hour *);
      time_since_last_message ~name:"Scheduled 7 days from now" ~now
        ~schedule_day:`Mon ~schedule_time:(9, 0, 0) ~expected:601200
      (* A week - 1h *);
    ]
end

let () = Alcotest.run "slack_bot" [ ("scheduling", Test_schedule.tests) ]
