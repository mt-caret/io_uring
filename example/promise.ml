open Core

(* this is an implmentation of coroutines based on io_uring's event loop,
 * along with a port of http.ml *)

let queue_depth = 4096

module Promise : sig
  type 'a t

  val create : f:(resolve:('a -> unit) -> unit) -> 'a t
  val resolve : 'a t -> 'a -> unit
  val upon : 'a t -> f:('a -> unit) -> unit
  val peek : 'a t -> 'a option
  val never : unit -> 'a t
  val run : ?debug:bool -> 'a t -> 'a

  val accept
    :  Unix.File_descr.t
    -> (Unix.File_descr.t * Unix.sockaddr, Unix.Error.t) Result.t t

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
  module Cell = struct
    type 'a t =
      { mutable cell : 'a option
      ; mutable callback : ('a -> unit) list
      }
    [@@deriving sexp, fields]

    let create = Fields.create
  end

  module User_data = struct
    type t =
      | Resolve : ('a * 'a Cell.t) -> t
      | Accept :
          { fd : Unix.File_descr.t
                (* TODO: the fact that we need to carry around a mutable reference to
                 * [queued_sockaddr] isn't very nice, and suggests that we should
                 * more closely mimic c-style reference passing. unsure whether
                 * lifetime safety is workable in that case. *)
          ; mutable queued_sockaddr : (Io_uring.Queued_sockaddr.t[@sexp.opaque]) option
          ; resolve : (Unix.File_descr.t * Unix.sockaddr, Unix.Error.t) Result.t -> unit
          }
          -> t
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
      print_s [%message "prepare" (t : t)];
      let io_uring = force io_uring in
      match t with
      | Resolve _ ->
        let sq_full = Io_uring.prepare_nop io_uring Io_uring.Sqe_flags.none t in
        if sq_full then raise_s [%message "promise: submission queue is full"]
      | Accept accept ->
        (match Io_uring.prepare_accept io_uring Io_uring.Sqe_flags.none accept.fd t with
        | None -> raise_s [%message "accept: submission queue is full"]
        | Some queued_sockaddr -> accept.queued_sockaddr <- Some queued_sockaddr)
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
      if res < 0 then Error (Unix.Error.of_system_int ~errno:(-res)) else Ok res
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
            | Resolve (x, cell) ->
              Cell.callback cell |> List.rev |> List.iter ~f:(fun f -> f x)
            | Accept { fd = _; queued_sockaddr; resolve } ->
              process_res res
              |> Result.map ~f:(fun res ->
                     match
                       Option.value_exn queued_sockaddr
                       |> Io_uring.Queued_sockaddr.thread_unsafe_get
                     with
                     | None ->
                       raise_s
                         [%message
                           "failed to find sockaddr when \
                            Queued_sockaddr.thread_unsafe_get"]
                     | Some sockaddr -> Unix.File_descr.of_int res, sockaddr)
              |> resolve
            | Recv { resolve; _ } -> process_res res |> resolve
            | Sendmsg { resolve; _ } -> process_res res |> resolve);
        let n = Io_uring.num_completions io_uring in
        print_s [%message "completions" (n : int)];
        Io_uring.clear_completions io_uring
      done
    ;;
  end

  module T = struct
    type 'a t = 'a Cell.t [@@deriving sexp]

    let peek = Cell.cell

    let resolve t x =
      match peek t with
      | None -> User_data.prepare (Resolve (x, t))
      | Some _ -> raise_s [%message "attempted to fill fulfulled process"]
    ;;

    let create ~f =
      let t = Cell.create ~cell:None ~callback:[] in
      f ~resolve:(resolve t);
      t
    ;;

    let never () = create ~f:(fun ~resolve:_ -> ())

    let upon t ~f =
      match peek t with
      | None -> t.callback <- f :: t.callback
      | Some x -> f x
    ;;

    let run ?debug t =
      let result = ref None in
      upon t ~f:(fun x -> result := Some x);
      User_data.submit_and_wait ?debug (fun () -> Option.is_none !result);
      Option.value_exn !result
    ;;

    let accept fd =
      create ~f:(fun ~resolve ->
          User_data.Accept { fd; queued_sockaddr = None; resolve } |> User_data.prepare)
    ;;

    let recv fd ?pos ?len buf =
      create ~f:(fun ~resolve ->
          User_data.Recv { fd; pos; len; buf; resolve } |> User_data.prepare)
    ;;

    let sendmsg fd iovecs =
      create ~f:(fun ~resolve ->
          User_data.Sendmsg { fd; iovecs; resolve } |> User_data.prepare)
    ;;

    let return x =
      let t = Cell.create ~cell:None ~callback:[] in
      resolve t x;
      t
    ;;

    let map t ~f =
      match peek t with
      | None -> create ~f:(fun ~resolve -> upon t ~f:(fun x -> resolve (f x)))
      | Some x -> return (f x)
    ;;

    let bind t ~f =
      match peek t with
      | None -> create ~f:(fun ~resolve -> upon t ~f:(fun x -> upon (f x) ~f:resolve))
      | Some x -> f x
    ;;
  end

  include T

  include Monad.Make (struct
    include T

    let map = `Custom map
  end)
end

module Handler = struct
  open Httpaf

  let request_handler reqd =
    match Reqd.request reqd with
    | { Request.meth = `POST; headers; _ } ->
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None -> "application/octet-stream"
          | Some x -> x
        in
        Response.create
          ~headers:
            (Headers.of_list [ "content-type", content_type; "connection", "close" ])
          `OK
      in
      let request_body = Reqd.request_body reqd in
      let response_body = Reqd.respond_with_streaming reqd response in
      let rec on_read buffer ~off ~len =
        Body.write_bigstring response_body buffer ~off ~len;
        Body.schedule_read request_body ~on_eof ~on_read
      and on_eof () = Body.close_writer response_body in
      Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
    | _ ->
      let headers = Headers.of_list [ "connection", "close" ] in
      Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) ""
  ;;

  let error_handler ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    (match error with
    | `Exn exn ->
      Body.write_string response_body (Exn.to_string exn);
      Body.write_string response_body "\n"
    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error));
    Body.close_writer response_body
  ;;
end

module Buffer = struct
  type t =
    { buffer : (Bigstring.t[@sexp.opaque])
    ; mutable off : int
    ; mutable len : int
    }
  [@@deriving sexp]

  let create size =
    let buffer = Bigstring.create size in
    { buffer; off = 0; len = 0 }
  ;;

  let compress t =
    if t.len = 0
    then (
      t.off <- 0;
      t.len <- 0)
    else if t.off > 0
    then (
      Bigstring.blit ~src:t.buffer ~src_pos:t.off ~dst:t.buffer ~dst_pos:0 ~len:t.len;
      t.off <- 0)
  ;;

  let read t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0
  ;;

  let start_write t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len)
  ;;

  let finish_writing t n = t.len <- t.len + n
end

let rec accept_loop fd ~handle_connection =
  print_s [%message "in accept_loop"];
  let open Promise.Let_syntax in
  let%bind result = Promise.accept fd in
  let%map () = accept_loop fd ~handle_connection
  and () =
    match result with
    | Ok conn -> handle_connection conn
    | Error error -> raise_s [%message "accept failed" (error : Unix.Error.t)]
  in
  ()
;;

let unwrap_result ~name = function
  | Ok x -> x
  | Error error -> raise_s [%message (name ^ " failed") (error : Unix.Error.t)]
;;

let to_core_iovec_array (iovecs : Bigstring.t Faraday.iovec list) =
  List.map iovecs ~f:(fun { buffer; off; len } ->
      Unix.IOVec.of_bigstring ~pos:off ~len buffer)
  |> List.to_array
;;

let run ?(config = Httpaf.Config.default) ~queue_depth:_ ~port ~backlog ~debug () =
  let sockfd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.setsockopt sockfd SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
  Unix.bind sockfd ~addr;
  Unix.listen sockfd ~backlog;
  accept_loop sockfd ~handle_connection:(fun (fd, _sockaddr) ->
      let conn =
        Handler.(Httpaf.Server_connection.create ~config ~error_handler request_handler)
      in
      let buf = Buffer.create config.read_buffer_size in
      let open Promise.Let_syntax in
      let rec reader_thread () =
        print_s [%message "in reader_thread loop"];
        match Httpaf.Server_connection.next_read_operation conn with
        | `Read ->
          let%bind () =
            Buffer.start_write buf ~f:(fun bigstring ~off ~len ->
                let%map result =
                  Promise.recv fd ~pos:off ~len bigstring >>| unwrap_result ~name:"recv"
                in
                print_s [%message "finished recv"];
                Buffer.finish_writing buf result;
                Buffer.read buf ~f:(fun bigstring ~off ~len ->
                    if result = 0
                    then Httpaf.Server_connection.read_eof conn bigstring ~off ~len
                    else Httpaf.Server_connection.read conn bigstring ~off ~len))
          in
          reader_thread ()
        | `Yield ->
          let%bind () = return () in
          reader_thread ()
        | `Close ->
          Unix.shutdown fd ~mode:SHUTDOWN_RECEIVE;
          return ()
      in
      let rec writer_thread () =
        print_s [%message "in writer_thread loop"];
        match Httpaf.Server_connection.next_write_operation conn with
        | `Write iovecs ->
          let%bind result =
            to_core_iovec_array iovecs
            |> Promise.sendmsg fd
            >>| unwrap_result ~name:"sendmsg"
          in
          Httpaf.Server_connection.report_write_result
            conn
            (if result = 0 then `Closed else `Ok result);
          writer_thread ()
        | `Yield ->
          let%bind () = return () in
          writer_thread ()
        | `Close _ ->
          Unix.shutdown fd ~mode:SHUTDOWN_SEND;
          return ()
      in
      let%map () = reader_thread ()
      and () = writer_thread () in
      ())
  |> Promise.run ~debug
;;

let () =
  Command.run
  @@ Command.basic ~summary:"toy http server with coroutines using io_uring"
  @@ let%map_open.Command queue_depth =
       flag
         "queue-depth"
         (optional_with_default 2048 int)
         ~doc:"INT submission completion queue depth"
     and port = flag "port" (required int) ~doc:" port to listen on"
     and backlog =
       flag
         "backlog"
         (optional_with_default 100 int)
         ~doc:"INT size of backlog for listen()"
     and debug = flag "debug" no_arg ~doc:"BOOL enable debug mode" in
     fun () -> run ~queue_depth ~port ~backlog ~debug ()
;;
