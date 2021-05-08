open Core
module File_descr = Unix.File_descr
module IOVec = Unix.IOVec

module Tag : sig
  type 'a t [@@deriving sexp]

  module Option : sig
    type 'a value = 'a t
    type 'a t

    val is_none : 'a t -> bool
    val to_option : 'a t -> 'a value option

    module Optional_syntax :
      Optional_syntax_intf.S1 with type 'a t := 'a t with type 'a value = 'a value
  end
end

module Poll_flags : sig
  (** An [Io_uring.Poll_flags.t] is an immutable set of poll(2) flags for which one can
          register interest in a file descriptor.  It is implemented as a bitmask, and
          so all operations (+, -, etc.) are constant time with no allocation.

          [sexp_of_t] produces a human-readable list of bits, e.g., "(in out)". *)
  type t [@@deriving sexp_of]

  include Flags.S with type t := t

  (** The names of the flags match the poll(2) man pages.  E.g. [in_] = "POLLIN",
         [out] = "POLLOUT", etc. *)

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

module Sqe_flags : sig
  type t [@@deriving sexp_of]

  include Flags.S with type t := t

  val none : t
  val fixed_file : t
  val io_drain : t
  val io_link : t
  val io_hardlink : t
  val async : t
  val buffer_select : t
end

module Queued_sockaddr : sig
  type t

  val thread_unsafe_get : t -> Unix.sockaddr option
end

type 'a t

val create : max_submission_entries:int -> max_completion_entries:int -> _ t
val close : 'a t -> unit
val prepare_nop : 'a t -> Sqe_flags.t -> 'a -> bool

val prepare_write
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> offset:int
  -> 'a
  -> bool

val prepare_read
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> offset:int
  -> 'a
  -> bool

val prepare_write
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> offset:int
  -> 'a
  -> bool

val prepare_writev
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> offset:int
  -> 'a
  -> bool

val prepare_readv
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> offset:int
  -> 'a
  -> bool

val prepare_send
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> 'a
  -> bool

val prepare_recv
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> 'a
  -> bool

val prepare_sendmsg
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> 'a
  -> bool

val prepare_recvmsg
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> 'a
  -> bool

(* TODO: test *)
val prepare_close : 'a t -> Sqe_flags.t -> File_descr.t -> 'a -> bool
val prepare_accept : 'a t -> Sqe_flags.t -> File_descr.t -> 'a -> Queued_sockaddr.t option

(** [poll_add] adds a file descriptor to listen to to the submission queue,
    and will take effect when [submit] is called. It returns an
    ['a Tag.Option.t] which is empty when the underlying submission queue is
    full and submission fails *)
val prepare_poll_add
  :  'a t
  -> Sqe_flags.t
  -> File_descr.t
  -> Poll_flags.t
  -> 'a
  -> 'a Tag.Option.t

val prepare_poll_remove : 'a t -> Sqe_flags.t -> 'a Tag.t -> bool
val submit : 'a t -> int

(* TOIMPL: fix doc *)

(** [wait] waits for events until [~timeout] has passed (in nanoseconds),
    then returns the tag given to it by [poll_add] and 0 if it has timed out.
    passing in 0 for [~timeout] will cause it return immediately, and a
    negative value will cause it to wait indefinitely. *)
val wait
  :  'a t
  -> timeout:
       [ `Never | `Immediately | `After of Time_ns.Span.t | `Until_completions of int ]
  -> unit

val wait_timeout_after : 'a t -> Time_ns.Span.t -> unit
val iter_completions : 'a t -> f:(user_data:'a -> res:int -> flags:int -> unit) -> unit
val clear_completions : 'a t -> unit
val num_completions : 'a t -> int
