open Core
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result
module Tag = Tag

[%%import "config.h"]

(* TOIMPL: possibly prove for functionality via io_uring_get_probe() *)
external flag_pollin : unit -> Int63.t = "poll_POLLIN_flag"
external flag_pollpri : unit -> Int63.t = "poll_POLLPRI_flag"
external flag_pollout : unit -> Int63.t = "poll_POLLOUT_flag"
external flag_pollerr : unit -> Int63.t = "poll_POLLERR_flag"
external flag_pollhup : unit -> Int63.t = "poll_POLLHUP_flag"

(* We use [Int63] rather than [Int] because these flags use 16 bits. *)
module Flags = struct
  let none = Int63.zero
  let in_ = flag_pollin ()
  let pri = flag_pollpri ()
  let out = flag_pollout ()
  let err = flag_pollerr ()
  let hup = flag_pollhup ()

  include Flags.Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let remove_zero_flags = false
    let known = [ in_, "in"; pri, "pri"; out, "out"; err, "err"; hup, "hup" ]
  end)
end

(* TOIMPL: flesh out the interface here *)
type _ io_uring

external create
  :  max_submission_entries:int
  -> max_completion_entries:int
  -> _ io_uring
  = "io_uring_queue_init_stub"

external close : _ io_uring -> unit = "io_uring_queue_exit_stub"
external nop : 'a io_uring -> 'a -> 'a Tag.Option.t = "io_uring_prep_nop_stub"

external write
  :  'a io_uring
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> 'a
  -> bool
  = "io_uring_prep_write_bytecode_stub" "io_uring_prep_write_stub"

external read
  :  'a io_uring
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> 'a
  -> bool
  = "io_uring_prep_read_bytecode_stub" "io_uring_prep_read_stub"

external poll_add
  :  'a io_uring
  -> File_descr.t
  -> Flags.t
  -> 'a
  -> 'a Tag.Option.t
  = "io_uring_prep_poll_add_stub"

external poll_remove : 'a io_uring -> 'a Tag.t -> bool = "io_uring_prep_poll_remove_stub"
external submit : _ io_uring -> int = "io_uring_submit_stub"

external wait_internal
  :  _ io_uring
  -> Bigstring.t
  -> timeout:Int63.t
  -> int
  = "io_uring_wait_stub"

let wait_timeout_after t buffer span =
  let timeout =
    if Time_ns.Span.(span <= zero) then Int63.zero else Time_ns.Span.to_int63_ns span
  in
  wait_internal t buffer ~timeout
;;

let wait t buffer ~timeout =
  match timeout with
  | `Never -> wait_internal t buffer ~timeout:(Int63.of_int (-1))
  | `Immediately -> wait_internal t buffer ~timeout:Int63.zero
  | `After span -> wait_timeout_after t buffer span
;;

type 'a t =
  { io_uring : 'a io_uring
  ; completion_buffer : Bigstring.t
  ; mutable completions : int
  }

external io_uring_sizeof_io_uring_cqe : unit -> int = "io_uring_sizeof_io_uring_cqe"
  [@@noalloc]

external io_uring_offsetof_user_data : unit -> int = "io_uring_offsetof_user_data"
  [@@noalloc]

external io_uring_offsetof_res : unit -> int = "io_uring_offsetof_res" [@@noalloc]
external io_uring_offsetof_flags : unit -> int = "io_uring_offsetof_flags" [@@noalloc]

let sizeof_io_uring_cqe = io_uring_sizeof_io_uring_cqe ()
let offsetof_user_data = io_uring_offsetof_user_data ()
let offsetof_res = io_uring_offsetof_res ()
let offsetof_flags = io_uring_offsetof_flags ()

(* we need to resort to a FFI call instead of using
 * [Bigstring.unsafe_get_uint64_le_exn] along with [Obj.magic], for example,
 * since the result will then be returned as an int value and be shifted left
 * by 1 bit *)
external unsafe_get_user_data : Bigstring.t -> int -> 'a = "io_uring_get_user_data"

let cqe_res buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_res)
;;

let cqe_flags buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_flags)
;;

let create ~max_submission_entries ~max_completion_entries =
  { io_uring = create ~max_submission_entries ~max_completion_entries
  ; completion_buffer =
      Bigstring.init (sizeof_io_uring_cqe * max_completion_entries) (Fn.const 'A')
  ; completions = 0
  }
;;

let close t = close t.io_uring
let nop t = nop t.io_uring

let write t fd ?(pos = 0) ?len bstr ~offset a =
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.write" ~pos ~len bstr;
  write t.io_uring fd ~pos ~len bstr ~offset a
;;

let read t fd ?(pos = 0) ?len bstr ~offset a =
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.read" ~pos ~len bstr;
  read t.io_uring fd ~pos ~len bstr ~offset a
;;

let poll_add t = poll_add t.io_uring
let poll_remove t = poll_remove t.io_uring

(*let writev t = writev t.io_uring*)

(* TOIMPL: add invariant that num_in_flight is always >= 0? *)
let submit t = submit t.io_uring

(* submit is automatcally called here, so I don't think it's possible to
 * accurately keep track of in-flight requests if we use liburing *)
let wait_timeout_after t span =
  t.completions <- wait_timeout_after t.io_uring t.completion_buffer span
;;

let wait t ~timeout = t.completions <- wait t.io_uring t.completion_buffer timeout

let iter_completions t ~f =
  (* print_s [%message "iter_completions" (t.completions : int)]; *)
  for i = 0 to t.completions - 1 do
    let user_data = unsafe_get_user_data t.completion_buffer i in
    let res = cqe_res t.completion_buffer i in
    let flags = cqe_flags t.completion_buffer i in
    f ~user_data ~res ~flags
  done
;;

external unsafe_clear_completions
  :  Bigstring.t
  -> int
  -> unit
  = "io_uring_clear_completions"

let clear_completions t =
  unsafe_clear_completions t.completion_buffer t.completions;
  t.completions <- 0
;;

let num_completions t = t.completions
