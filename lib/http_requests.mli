(** Makes http requests to the slack API. *)

type t

val v : token:string -> unit -> t

val write_matches :
  channel:string -> string -> t -> (Yojson.Safe.t, string) result Lwt.t

val get_reactions :
  channel:string -> timestamp:string -> t -> (string list, string) result Lwt.t
(** Query the reactions of a message. Identify a specific message by its timestamp. *)

val write_opt_in_message : channel:string -> t -> (string, string) result Lwt.t
