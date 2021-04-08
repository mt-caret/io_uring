open Core
module File_descr = Unix.File_descr
module Tag = Tag

module Flags : sig
  (** An [Io_uring.Flags.t] is an immutable set of poll(2) flags for which one can
          register interest in a file descriptor.  It is implemented as a bitmask, and
          so all operations (+, -, etc.) are constant time with no allocation.

          [sexp_of_t] produces a human-readable list of bits, e.g., "(in out)". *)
  type t [@@deriving sexp_of]

  include Flags.S with type t := t

  (** The names of the flags match the poll(2) man pages.  E.g. [in_] = "POLLIN",
         [out] = "POLLOUT", etc. *)

  (** Associated fd is readable                      *)
  val none : t

  (** Associated fd is readable                      *)
  val in_ : t

  (** Urgent data available                          *)
  val pri : t

  (** Associated fd is writable                      *)
  val out : t

  (** Error condition (always on, no need to set it) *)
  val err : t

  (** Hang up happened (always on)                   *)
  val hup : t
end

type 'a t

val create : max_submission_entries:int -> max_completion_entries:int -> _ t
val close : 'a t -> unit

(** [poll_add] adds a file descriptor to listen to to the submission queue,
        and will take effect when [submit] is called. *)
val poll_add : 'a t -> File_descr.t -> Flags.t -> 'a -> 'a Tag.Option.t

val poll_remove : 'a t -> 'a Tag.t -> bool
val submit : 'a t -> int

(* TOIMPL: fix doc *)

(** [wait] waits for events until [~timeout] has passed (in nanoseconds),
    then returns the tag given to it by [poll_add] and 0 if it has timed out.
    passing in 0 for [~timeout] will cause it return immediately, and a
    negative value will cause it to wait indefinitely. *)
val wait : 'a t -> timeout:[ `Never | `Immediately | `After of Time_ns.Span.t ] -> unit

val wait_timeout_after : 'a t -> Time_ns.Span.t -> unit
val iter_completions : 'a t -> f:(user_data:'a -> res:int -> flags:int -> unit) -> unit
val clear_completions : 'a t -> unit
