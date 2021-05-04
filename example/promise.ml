open Core

let queue_depth = 2048

module Promise : sig
  type 'a t

  val resolve : 'a t -> 'a -> unit
  val upon : 'a t -> f:('a -> unit) -> unit

  include Monad.S with type 'a t := 'a t
end = struct
  module User_data = struct
    type t = Resolved : ('a * ('a -> unit)) -> t [@@deriving sexp_of]

    let io_uring =
      lazy
        (Io_uring.create
           ~max_submission_entries:queue_depth
           ~max_completion_entries:queue_depth)
    ;;

    let prepare t =
      match t with
      | Resolved _ ->
        let sq_full = Io_uring.prepare_nop (force io_uring) Io_uring.Sqe_flags.none t in
        if sq_full then raise_s [%message "submission queue is full"]
    ;;

    let submit_and_wait ?(debug = false) () =
      let io_uring = force io_uring in
      while true do
        let num_submitted = Io_uring.submit io_uring in
        if debug then print_s [%message "" (num_submitted : int)];
        Io_uring.wait io_uring ~timeout:`Never;
        Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
            if debug
            then print_s [%message "completion" (user_data : t) (res : int) (flags : int)];
            match user_data with
            | Resolved (x, f) -> f x);
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

    let create ~f = ref (Cell.Empty f)

    let resolve t x =
      match !t with
      | Cell.Empty f ->
        t := Full x;
        User_data.prepare (Resolved (x, f))
      | Full _ -> raise_s [%message "attempted to fill fulfulled process"]
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
  end

  include T

  include Monad.Make (struct
    include T

    let return x = ref (Cell.Full x)

    let map t ~f =
      match !t with
      | Cell.Empty _ ->
        let t' = create ~f:ignore in
        upon t ~f:(fun x -> resolve t' (f x));
        t'
      | Full x -> return (f x)
    ;;

    let bind t ~f =
      match !t with
      | Cell.Empty g ->
        let t' = create ~f:ignore in
        upon t ~f:(fun x -> upon (f x) ~f:(resolve t'));
        t'
      | Full x -> f x
    ;;

    let map = `Custom map
  end)
end

let () = print_s [%message "Hello, World!"]
