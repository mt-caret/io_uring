open Core
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

module Tag = struct
  type 'a t = int [@@deriving sexp]

  module Option = struct
    module T = struct
      type 'a value = 'a t
      type 'a t = int

      let is_none t = t = -1
      let unsafe_value t = t
      let to_option t = if is_none t then None else Some (unsafe_value t)
    end

    include T

    module Optional_syntax = struct
      include T

      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = unsafe_value
      end
    end
  end
end

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
external nop : 'a io_uring -> user_data:int -> bool = "io_uring_prep_nop_stub" [@@noalloc]

external write
  :  'a io_uring
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_write_bytecode_stub" "io_uring_prep_write_stub"
  [@@noalloc]

external read
  :  'a io_uring
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_read_bytecode_stub" "io_uring_prep_read_stub"
  [@@noalloc]

external poll_add
  :  'a io_uring
  -> File_descr.t
  -> Flags.t
  -> user_data:int
  -> bool
  = "io_uring_prep_poll_add_stub"
  [@@noalloc]

external poll_remove
  :  'a io_uring
  -> user_data:int
  -> bool
  = "io_uring_prep_poll_remove_stub"
  [@@noalloc]

external submit : _ io_uring -> int = "io_uring_submit_stub" [@@noalloc]

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
  ; mutable head : int
  ; freelist : int Array.t
  ; user_data : 'a Array.t
  }

let are_slots_full t = t.head = -1

let alloc_user_data (type a) (t : a t) (a : a) sq_full =
  if not sq_full
  then (
    assert (t.head <> -1);
    let next_index = t.freelist.(t.head) in
    (*print_s [%message "alloc_user_data" (t.head : int) (next_index : int)];*)
    t.freelist.(t.head) <- -1;
    t.user_data.(t.head) <- a;
    t.head <- (if next_index = t.head then -1 else next_index));
  sq_full
;;

let free_user_data t index =
  t.freelist.(index) <- (if t.head = -1 then index else t.head);
  t.head <- index;
  t.user_data.(index) <- Obj.magic 0
;;

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

let cqe_res buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_res)
;;

let cqe_flags buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_flags)
;;

let create ~max_submission_entries ~max_completion_entries =
  let user_data_slots = max_submission_entries + max_completion_entries in
  { io_uring = create ~max_submission_entries ~max_completion_entries
  ; completion_buffer =
      Bigstring.init (sizeof_io_uring_cqe * max_completion_entries) (Fn.const 'A')
  ; completions = 0
  ; head = 0
  ; freelist = Array.init user_data_slots (fun i -> Int.min (i + 1) (user_data_slots - 1))
  ; user_data = Array.init user_data_slots (fun _ -> Obj.magic 0)
  }
;;

let close t = close t.io_uring
let nop t a = are_slots_full t || nop t.io_uring ~user_data:t.head |> alloc_user_data t a

let write t fd ?(pos = 0) ?len bstr ~offset a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.write" ~pos ~len bstr;
  write t.io_uring fd ~pos ~len bstr ~offset ~user_data:t.head |> alloc_user_data t a
;;

let read t fd ?(pos = 0) ?len bstr ~offset a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.read" ~pos ~len bstr;
  read t.io_uring fd ~pos ~len bstr ~offset ~user_data:t.head |> alloc_user_data t a
;;

let poll_add t fd flags a =
  if are_slots_full t
  then -1
  else (
    let index = t.head in
    let sq_full = poll_add t.io_uring fd flags ~user_data:t.head |> alloc_user_data t a in
    if sq_full then -1 else index)
;;

let poll_remove t tag = are_slots_full t || poll_remove t.io_uring ~user_data:tag

(*let writev t = writev t.io_uring*)

(* TOIMPL: add invariant that num_in_flight is always >= 0? *)
let submit t = submit t.io_uring

(* submit is automatcally called here, so I don't think it's possible to
 * accurately keep track of in-flight requests if we use liburing *)
let wait_timeout_after t span =
  t.completions <- wait_timeout_after t.io_uring t.completion_buffer span
;;

let wait t ~timeout = t.completions <- wait t.io_uring t.completion_buffer timeout

external io_uring_get_user_data : Bigstring.t -> int -> int = "io_uring_get_user_data"
  [@@noalloc]

let iter_completions t ~f =
  for i = 0 to t.completions - 1 do
    let index = io_uring_get_user_data t.completion_buffer i in
    let user_data = t.user_data.(index) in
    let res = cqe_res t.completion_buffer i in
    let flags = cqe_flags t.completion_buffer i in
    f ~user_data ~res ~flags
  done
;;

let clear_completions t =
  for i = 0 to t.completions - 1 do
    io_uring_get_user_data t.completion_buffer i |> free_user_data t
  done;
  t.completions <- 0
;;

let num_completions t = t.completions
