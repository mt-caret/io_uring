open Core
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result
module IOVec = Unix.IOVec

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

(* We use [Int63] rather than [Int] because these flags use 16 bits. *)
module Poll_flags = struct
  external flag_pollin : unit -> Int63.t = "poll_POLLIN_flag"
  external flag_pollpri : unit -> Int63.t = "poll_POLLPRI_flag"
  external flag_pollout : unit -> Int63.t = "poll_POLLOUT_flag"
  external flag_pollerr : unit -> Int63.t = "poll_POLLERR_flag"
  external flag_pollhup : unit -> Int63.t = "poll_POLLHUP_flag"

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

module Sqe_flags = struct
  external flag_fixed_file : unit -> Int63.t = "sqe_IOSQE_FIXED_FILE_flag"
  external flag_io_drain : unit -> Int63.t = "sqe_IOSQE_IO_DRAIN_flag"
  external flag_io_link : unit -> Int63.t = "sqe_IOSQE_IO_LINK_flag"
  external flag_io_hardlink : unit -> Int63.t = "sqe_IOSQE_IO_HARDLINK_flag"
  external flag_async : unit -> Int63.t = "sqe_IOSQE_ASYNC_flag"
  external flag_buffer_select : unit -> Int63.t = "sqe_IOSQE_BUFFER_SELECT_flag"

  let none = Int63.zero
  let fixed_file = flag_fixed_file ()
  let io_drain = flag_io_drain ()
  let io_link = flag_io_link ()
  let io_hardlink = flag_io_hardlink ()
  let async = flag_async ()
  let buffer_select = flag_buffer_select ()

  include Flags.Make (struct
    let allow_intersecting = true
    let should_print_error = true
    let remove_zero_flags = false

    let known =
      [ fixed_file, "fixed_file"
      ; io_drain, "io_drain"
      ; io_link, "io_link"
      ; io_hardlink, "io_hardlink"
      ; async, "async"
      ; buffer_select, "buffer_select"
      ]
    ;;
  end)
end

module Queued_sockaddr = struct
  type t

  external thread_unsafe_get : t -> Unix.sockaddr option = "io_uring_get_sockaddr"
  external unsafe_free : t -> unit = "io_uring_free_sockaddr"
end

(* TOIMPL: flesh out the interface here *)
type _ io_uring

external create
  :  max_submission_entries:int
  -> max_completion_entries:int
  -> _ io_uring
  = "io_uring_queue_init_stub"

external close : _ io_uring -> unit = "io_uring_queue_exit_stub"

external prepare_nop
  :  'a io_uring
  -> Sqe_flags.t
  -> user_data:int
  -> bool
  = "io_uring_prep_nop_stub"
  [@@noalloc]

external prepare_write
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_write_bytecode_stub" "io_uring_prep_write_stub"
  [@@noalloc]

external prepare_read
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_read_bytecode_stub" "io_uring_prep_read_stub"
  [@@noalloc]

external prepare_writev
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> count:int
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_writev_bytecode_stub" "io_uring_prep_writev_stub"
  [@@noalloc]

external prepare_readv
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> count:int
  -> offset:int
  -> user_data:int
  -> bool
  = "io_uring_prep_readv_bytecode_stub" "io_uring_prep_readv_stub"
  [@@noalloc]

external prepare_send
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> user_data:int
  -> bool
  = "io_uring_prep_send_bytecode_stub" "io_uring_prep_send_stub"
  [@@noalloc]

external prepare_recv
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> pos:int
  -> len:int
  -> Bigstring.t
  -> user_data:int
  -> bool
  = "io_uring_prep_recv_bytecode_stub" "io_uring_prep_recv_stub"
  [@@noalloc]

external prepare_sendmsg
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> Bigstring.t IOVec.t array
  -> count:int
  -> user_data:int
  -> bool
  = "io_uring_prep_sendmsg_bytecode_stub" "io_uring_prep_sendmsg_stub"
  [@@noalloc]

external prepare_close
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> user_data:int
  -> bool
  = "io_uring_prep_close_stub"
  [@@noaloc]

external prepare_accept
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> user_data:int
  -> Queued_sockaddr.t option
  = "io_uring_prep_accept_stub"

external prepare_poll_add
  :  'a io_uring
  -> Sqe_flags.t
  -> File_descr.t
  -> Poll_flags.t
  -> user_data:int
  -> bool
  = "io_uring_prep_poll_add_stub"
  [@@noalloc]

external prepare_poll_remove
  :  'a io_uring
  -> Sqe_flags.t
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
    (* print_s [%message "alloc_user_data" (t.head : int) (next_index : int)]; *)
    t.freelist.(t.head) <- -1;
    t.user_data.(t.head) <- a;
    t.head <- (if next_index = t.head then -1 else next_index));
  sq_full
;;

let free_user_data t index =
  (* print_s [%message "free_user_data" (t.head : int) (index : int)]; *)
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

let prepare_nop t sqe_flags a =
  are_slots_full t
  || prepare_nop t.io_uring sqe_flags ~user_data:t.head |> alloc_user_data t a
;;

let prepare_write t sqe_flags fd ?(pos = 0) ?len bstr ~offset a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.write" ~pos ~len bstr;
  prepare_write t.io_uring sqe_flags fd ~pos ~len bstr ~offset ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_read t sqe_flags fd ?(pos = 0) ?len bstr ~offset a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.read" ~pos ~len bstr;
  prepare_read t.io_uring sqe_flags fd ~pos ~len bstr ~offset ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_writev t sqe_flags fd iovecs ~offset a =
  are_slots_full t
  ||
  let count = Array.length iovecs in
  prepare_writev t.io_uring sqe_flags fd iovecs ~count ~offset ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_readv t sqe_flags fd iovecs ~offset a =
  are_slots_full t
  ||
  let count = Array.length iovecs in
  prepare_readv t.io_uring sqe_flags fd iovecs ~count ~offset ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_send t sqe_flags fd ?(pos = 0) ?len bstr a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.send" ~pos ~len bstr;
  prepare_send t.io_uring sqe_flags fd ~pos ~len bstr ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_recv t sqe_flags fd ?(pos = 0) ?len bstr a =
  are_slots_full t
  ||
  let len = Bigstring.get_opt_len bstr ~pos len in
  Bigstring.check_args ~loc:"io_uring.recv" ~pos ~len bstr;
  prepare_recv t.io_uring sqe_flags fd ~pos ~len bstr ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_sendmsg t sqe_flags fd iovecs a =
  are_slots_full t
  ||
  let count = Array.length iovecs in
  prepare_sendmsg t.io_uring sqe_flags fd iovecs ~count ~user_data:t.head
  |> alloc_user_data t a
;;

let prepare_close t sqe_flags fd a =
  are_slots_full t
  || prepare_close t.io_uring sqe_flags fd ~user_data:t.head |> alloc_user_data t a
;;

let prepare_accept t sqe_flags fd a =
  if are_slots_full t
  then None
  else (
    let res = prepare_accept t.io_uring sqe_flags fd ~user_data:t.head in
    Option.iter res ~f:(fun queued_sockaddr ->
        Gc.Expert.add_finalizer_exn queued_sockaddr Queued_sockaddr.unsafe_free);
    ignore (alloc_user_data t a (Option.is_none res) : bool);
    res)
;;

let prepare_poll_add t sqe_flags fd flags a =
  if are_slots_full t
  then -1
  else (
    let index = t.head in
    let sq_full =
      prepare_poll_add t.io_uring sqe_flags fd flags ~user_data:t.head
      |> alloc_user_data t a
    in
    if sq_full then -1 else index)
;;

let prepare_poll_remove t sqe_flags tag =
  are_slots_full t || prepare_poll_remove t.io_uring sqe_flags ~user_data:tag
;;

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
