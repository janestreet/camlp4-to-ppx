open Camlp4.PreCast

(** Input file contents *)
val file_contents : string

(** Add a substitution to the input file. When {!main} is called the program will print
    the input with all substitutions applied. *)
val add_subst : start:int -> stop:int -> repl:string -> unit

val replace      : Loc.t -> string -> unit
val print_before : Loc.t -> string -> unit
val print_after  : Loc.t -> string -> unit

(** Replace the given location by nothing, and if the location is preceded and followed by
    a space, remove the space after as well. *)
val erase_keyword : Loc.t -> unit

(** If the last character at given location is a semi-colon, erase it. *)
val skip_trailing_semi : Loc.t -> unit

type payload_kind = Str | Typ | Pat

(** We need to know what kind of payload to use to replace quotations by extensions
    (i.e. what to put after the id: a ':', a '?' or nothing).

    By default quotations are expected to contain a type.
*)
val set_payload_kind : quotation_name:string -> payload_kind -> unit

val main : unit -> unit
