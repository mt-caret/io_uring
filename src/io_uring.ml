open Core
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

(* We use [Int63] rather than [Int] because these flags use 16 bits. *)

module Poll_flags (Flag_values : sig
  val in_ : Int63.t
  val pri : Int63.t
  val out : Int63.t
  val err : Int63.t
  val hup : Int63.t
end) =
struct end

[%%import "config.h"]

(* TOIMPL: possibly prove for functionality via io_uring_get_probe() *)
external flag_pollin : unit -> Int63.t = "core_linux_poll_POLLIN_flag"
external flag_pollpri : unit -> Int63.t = "core_linux_poll_POLLPRI_flag"
external flag_pollout : unit -> Int63.t = "core_linux_poll_POLLOUT_flag"
external flag_pollerr : unit -> Int63.t = "core_linux_poll_POLLERR_flag"
external flag_pollhup : unit -> Int63.t = "core_linux_poll_POLLHUP_flag"

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

module Kind = struct
  type _ t =
    | Poll : [ `Poll ] t
    | Read : [ `Read ] t
    | Writev : [ `Writev ] t
end

module User_data = struct
  type 'a t = Int63.t [@@deriving sexp]

  let of_int = Int63.of_int

  (* TOIMPL: it seems like there should be a more sane way to do this... *)
  let kind (type a) (t : a t) : a Kind.t = Obj.magic Kind.Poll

  let file_descr t =
    Int63.bit_and t (Int63.of_int 0xffff_ffff) |> Int63.to_int_exn |> File_descr.of_int
  ;;

  let flags t = Int63.shift_right t 32

  module Pretty = struct
    type t =
      { file_descr : File_descr.t
      ; flags : Flags.t
      }
    [@@deriving sexp_of]

    let create t = { file_descr = file_descr t; flags = flags t }
  end

  let sexp_of_t _ t = Pretty.sexp_of_t (Pretty.create t)
end

(* TOIMPL: flesh out the interface here *)
type _ io_uring

external create
  :  max_submission_entries:Int32.t
  -> max_completion_entries:Int32.t
  -> _ io_uring
  = "core_linux_io_uring_queue_init"

external close : _ io_uring -> unit = "core_linux_io_uring_queue_exit"

external poll_add
  :  [> `Poll ] io_uring
  -> File_descr.t
  -> Flags.t
  -> bool
  = "core_linux_io_uring_prep_poll_add"

external poll_remove
  :  [> `Poll ] io_uring
  -> File_descr.t
  -> Flags.t
  -> bool
  = "core_linux_io_uring_prep_poll_remove"

external unsafe_writev
  :  [> `Writev ] io_uring
  -> File_descr.t
  -> Bigstring.t Unix.IOVec.t array
  -> int
  -> bool
  = "core_linux_io_uring_prep_writev"

let writev t fd iovecs =
  let count = Array.length iovecs in
  unsafe_writev t fd iovecs count
;;

external submit : _ io_uring -> Int63.t = "core_linux_io_uring_submit"

external wait_internal
  :  _ io_uring
  -> Bigstring.t
  -> timeout:Int63.t
  -> int
  = "core_linux_io_uring_wait"

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
        (* TOIMPL: instead of allcating list, keep a bigstring around *)
  ; completion_buffer : Bigstring.t
  ; mutable completions : int
  }

external io_uring_sizeof_io_uring_cqe
  :  unit
  -> int
  = "core_linux_io_uring_sizeof_io_uring_cqe"
  [@@noalloc]

external io_uring_offsetof_user_data
  :  unit
  -> int
  = "core_linux_io_uring_offsetof_user_data"
  [@@noalloc]

external io_uring_offsetof_res : unit -> int = "core_linux_io_uring_offsetof_res"
  [@@noalloc]

external io_uring_offsetof_flags : unit -> int = "core_linux_io_uring_offsetof_flags"
  [@@noalloc]

let sizeof_io_uring_cqe = io_uring_sizeof_io_uring_cqe ()
let offsetof_user_data = io_uring_offsetof_user_data ()
let offsetof_res = io_uring_offsetof_res ()
let offsetof_flags = io_uring_offsetof_flags ()

let cqe_user_data buf i =
  Bigstring.unsafe_get_int64_le_trunc
    buf
    ~pos:((i * sizeof_io_uring_cqe) + offsetof_user_data)
  |> User_data.of_int
;;

let cqe_res buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_res)
;;

let cqe_flags buf i =
  Bigstring.unsafe_get_int32_le buf ~pos:((i * sizeof_io_uring_cqe) + offsetof_flags)
;;

let create ~max_submission_entries ~max_completion_entries =
  { io_uring = create ~max_submission_entries ~max_completion_entries
  ; completion_buffer =
      (* Bigstring.create (sizeof_io_uring_cqe * Int32.to_int_exn max_completion_entries) *)
      Bigstring.init
        (sizeof_io_uring_cqe * Int32.to_int_exn max_completion_entries)
        (Fn.const 'A')
  ; completions = 0
  }
;;

let close t = close t.io_uring
let poll_add t = poll_add t.io_uring
let poll_remove t = poll_remove t.io_uring
let writev t = writev t.io_uring

(* TOIMPL: add invariant that num_in_flight is always >= 0? *)
let submit t = submit t.io_uring |> Int63.to_int_exn

(* submit is automatcally called here, so I don't think it's possible to
 * accurately keep track of in-flight requests if we use liburing *)
let wait_timeout_after t span =
  t.completions <- wait_timeout_after t.io_uring t.completion_buffer span
;;

let wait t ~timeout = t.completions <- wait t.io_uring t.completion_buffer timeout

let iter_completions t ~f =
  (*print_s [%message "iter_completions" (t.completions : int)];*)
  for i = 0 to t.completions - 1 do
    let user_data = cqe_user_data t.completion_buffer i in
    let res = cqe_res t.completion_buffer i in
    let flags = cqe_flags t.completion_buffer i in
    (*(match User_data.kind user_data with
      | Poll ->
        let file_descr = User_data.file_descr user_data in
        let flags_ = User_data.flags user_data in
        print_s [%message "iter" (i : int) (file_descr : File_descr.t) (flags_ : Flags.t) (res : int) (flags : int)]);*)
    f ~user_data ~res ~flags
  done
;;

module Expert = struct
  let clear_completions t = t.completions <- 0
end
