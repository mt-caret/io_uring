open Core

let queue_depth = 2048

module Promise : sig
  type 'a t

  val create : f:(resolve:('a -> unit) -> unit) -> 'a t
  val resolve : 'a t -> 'a -> unit
  val upon : 'a t -> f:('a -> unit) -> unit
  val peek : 'a t -> 'a option
  val run : ?debug:bool -> 'a t -> 'a

  val recv
    :  Unix.File_descr.t
    -> ?pos:int
    -> ?len:int
    -> Bigstring.t
    -> (int, Unix.Error.t) Result.t t

  val sendmsg
    :  Unix.File_descr.t
    -> Bigstring.t Unix.IOVec.t array
    -> (int, Unix.Error.t) Result.t t

  include Monad.S with type 'a t := 'a t
end = struct
  module User_data = struct
    type t =
      | Resolved : ('a * ('a -> unit)) -> t
      | Recv :
          { fd : Unix.File_descr.t
          ; buf : (Bigstring.t[@sexp.opaque])
          ; pos : int option
          ; len : int option
          ; resolve : (int, Unix.Error.t) Result.t -> unit
          }
          -> t
      | Sendmsg :
          { fd : Unix.File_descr.t
          ; iovecs : (Bigstring.t[@sexp.opaque]) Unix.IOVec.t array
          ; resolve : (int, Unix.Error.t) Result.t -> unit
          }
          -> t
    [@@deriving sexp_of]

    let io_uring =
      lazy
        (Io_uring.create
           ~max_submission_entries:queue_depth
           ~max_completion_entries:queue_depth)
    ;;

    let prepare t =
      let io_uring = force io_uring in
      match t with
      | Resolved _ ->
        let sq_full = Io_uring.prepare_nop io_uring Io_uring.Sqe_flags.none t in
        if sq_full then raise_s [%message "promise: submission queue is full"]
      | Recv { fd; buf; pos; len; resolve = _ } ->
        let sq_full =
          Io_uring.prepare_recv io_uring Io_uring.Sqe_flags.none fd ?pos ?len buf t
        in
        if sq_full then raise_s [%message "recv: submission queue is full"]
      | Sendmsg { fd; iovecs; resolve = _ } ->
        let sq_full =
          Io_uring.prepare_sendmsg io_uring Io_uring.Sqe_flags.none fd iovecs t
        in
        if sq_full then raise_s [%message "sendmsg: submission queue is full"]
    ;;

    let process_res res =
      if res < 0 then Error (Unix.Error.of_system_int (-res)) else Ok res
    ;;

    (* TODO: ideally, we should probably keep track of submissions and wait
     * until everything has finished *)
    let submit_and_wait ?(debug = false) loop =
      let io_uring = force io_uring in
      while loop () do
        let num_submitted = Io_uring.submit io_uring in
        if debug then print_s [%message "" (num_submitted : int)];
        Io_uring.wait io_uring ~timeout:`Never;
        Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
            if debug
            then print_s [%message "completion" (user_data : t) (res : int) (flags : int)];
            match user_data with
            | Resolved (x, f) -> f x
            | Recv { resolve; _ } -> process_res res |> resolve
            | Sendmsg { resolve; _ } -> process_res res |> resolve);
        Io_uring.clear_completions io_uring
      done
    ;;
  end

  module Cell = struct
    type 'a t =
      | Empty of ('a -> unit)
      | Full of 'a
  end

  module T = struct
    type 'a t = 'a Cell.t ref

    let resolve t x =
      match !t with
      | Cell.Empty f ->
        t := Full x;
        User_data.prepare (Resolved (x, f))
      | Full _ -> raise_s [%message "attempted to fill fulfulled process"]
    ;;

    let create ~f =
      let t = ref (Cell.Empty ignore) in
      f ~resolve:(resolve t);
      t
    ;;

    let upon t ~f =
      match !t with
      | Cell.Empty g ->
        let g' x =
          g x;
          f x
        in
        t := Empty g'
      | Full x -> f x
    ;;

    let peek t =
      match !t with
      | Cell.Empty _ -> None
      | Full x -> Some x
    ;;

    let run ?debug t =
      let result = ref None in
      upon t ~f:(fun x -> result := Some x);
      User_data.submit_and_wait ?debug (fun () -> Option.is_none !result);
      Option.value_exn !result
    ;;

    let recv fd ?pos ?len buf =
      create ~f:(fun ~resolve ->
          User_data.Recv { fd; pos; len; buf; resolve } |> User_data.prepare)
    ;;

    let sendmsg fd iovecs =
      create ~f:(fun ~resolve ->
          User_data.Sendmsg { fd; iovecs; resolve } |> User_data.prepare)
    ;;
  end

  include T

  include Monad.Make (struct
    include T

    let return x = ref (Cell.Full x)

    let map t ~f =
      match !t with
      | Cell.Empty _ -> create ~f:(fun ~resolve -> upon t ~f:(fun x -> resolve (f x)))
      | Full x -> return (f x)
    ;;

    let bind t ~f =
      match !t with
      | Cell.Empty g ->
        create ~f:(fun ~resolve -> upon t ~f:(fun x -> upon (f x) ~f:resolve))
      | Full x -> f x
    ;;

    let map = `Custom map
  end)
end

let () = print_s [%message "Hello, World!"]
