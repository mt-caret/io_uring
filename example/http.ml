open Core

(* a (wip) port of https://github.com/inhabitedtype/httpaf/blob/master/examples/lib/httpaf_examples.ml *)

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

module User_data = struct
  module Reader_context = struct
    type t =
      { conn : (Httpaf.Server_connection.t[@sexp.opaque])
      ; fd : Unix.File_descr.t
      ; buf : Buffer.t
      }
    [@@deriving sexp_of]
  end

  type t =
    | Accept
    | Recv of Reader_context.t
    | Yield_reader of Reader_context.t
    | Sendmsg of
        { conn : (Httpaf.Server_connection.t[@sexp.opaque])
        ; fd : Unix.File_descr.t
        ; iovecs : (Bigstring.t[@sexp.opaque]) Unix.IOVec.t array
        }
    | Yield_writer of
        { conn : (Httpaf.Server_connection.t[@sexp.opaque])
        ; fd : Unix.File_descr.t
        }
  [@@deriving sexp_of]

  let prepare t ~io_uring ~sockfd ~queued_sockaddr_ref =
    match t with
    | Accept ->
      let queued_sockaddr =
        Io_uring.prepare_accept io_uring Io_uring.Sqe_flags.none sockfd t
      in
      queued_sockaddr_ref
        := (match queued_sockaddr with
           | None -> raise_s [%message "accept: submission queue is full"]
           | Some queued_sockaddr -> Some queued_sockaddr)
    | Recv { conn = _; fd; buf } ->
      let sq_full =
        Buffer.start_write buf ~f:(fun bigstring ~off ~len ->
            Io_uring.prepare_recv
              io_uring
              Io_uring.Sqe_flags.none
              fd
              ~pos:off
              ~len
              bigstring
              t)
      in
      if sq_full then raise_s [%message "recv: submission queue is full"]
    | Yield_reader _ ->
      (* we use nops when yielding to simplify logic *)
      let sq_full = Io_uring.prepare_nop io_uring Io_uring.Sqe_flags.none t in
      if sq_full then raise_s [%message "nop: submission queue is full"]
    | Sendmsg { conn = _; fd; iovecs } ->
      let sq_full =
        Io_uring.prepare_sendmsg io_uring Io_uring.Sqe_flags.none fd iovecs t
      in
      if sq_full then raise_s [%message "sendmsg: submission queue is full"]
    | Yield_writer _ ->
      let sq_full = Io_uring.prepare_nop io_uring Io_uring.Sqe_flags.none t in
      if sq_full then raise_s [%message "nop: submission queue is full"]
  ;;
end

let to_core_iovec_array (iovecs : Bigstring.t Faraday.iovec list) =
  List.map iovecs ~f:(fun { buffer; off; len } ->
      Unix.IOVec.of_bigstring ~pos:off ~len buffer)
  |> List.to_array
;;

let run ?(config = Httpaf.Config.default) ~queue_depth ~port ~backlog () =
  let sockfd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.setsockopt sockfd SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
  Unix.bind sockfd ~addr;
  Unix.listen sockfd ~backlog;
  let io_uring =
    Io_uring.create
      ~max_submission_entries:queue_depth
      ~max_completion_entries:(queue_depth * 2)
  in
  let queued_sockaddr_ref = ref None in
  let prepare = User_data.prepare ~io_uring ~sockfd ~queued_sockaddr_ref in
  prepare User_data.Accept;
  let reader_thread ~conn ~fd ~buf =
    match Httpaf.Server_connection.next_read_operation conn with
    | `Read -> User_data.Recv { conn; fd; buf } |> prepare
    | `Yield ->
      Httpaf.Server_connection.yield_reader conn (fun () ->
          User_data.Yield_reader { conn; fd; buf } |> prepare)
    | `Close -> Unix.shutdown fd SHUTDOWN_RECEIVE
  in
  let writer_thread ~conn ~fd =
    match Httpaf.Server_connection.next_write_operation conn with
    | `Write iovecs ->
      User_data.Sendmsg { conn; fd; iovecs = to_core_iovec_array iovecs } |> prepare
    | `Yield ->
      Httpaf.Server_connection.yield_writer conn (fun () ->
          User_data.Yield_writer { conn; fd } |> prepare)
    | `Close _ -> Unix.shutdown fd SHUTDOWN_SEND
  in
  while true do
    let (_ : int) = Io_uring.submit io_uring in
    Io_uring.wait io_uring ~timeout:`Never;
    Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
        print_s [%message "" (user_data : User_data.t) (res : int) (flags : int)];
        match user_data with
        | Accept ->
          if res < 0 then Unix.unix_error (-res) "Io_uring.accept" "";
          let sockaddr =
            Option.value_exn !queued_sockaddr_ref
            |> Io_uring.Queued_sockaddr.thread_unsafe_get
            |> Option.value_exn
          in
          print_s [%message "client connected" (sockaddr : Unix.sockaddr)];
          prepare User_data.Accept;
          let conn =
            Handler.(
              Httpaf.Server_connection.create ~config ~error_handler request_handler)
          in
          let fd = Unix.File_descr.of_int res in
          let buf = Buffer.create config.read_buffer_size in
          reader_thread ~conn ~fd ~buf;
          writer_thread ~conn ~fd
        | Recv { conn; fd; buf } ->
          (* TODO: fix handling? *)
          if res < 0 then Unix.unix_error (-res) "Io_uring.recv" "";
          Buffer.finish_writing buf res;
          Buffer.read buf ~f:(fun bigstring ~off ~len ->
              if res = 0
              then Httpaf.Server_connection.read_eof conn bigstring ~off ~len
              else Httpaf.Server_connection.read conn bigstring ~off ~len);
          reader_thread ~conn ~fd ~buf
        | Yield_reader { conn; fd; buf } -> reader_thread ~conn ~fd ~buf
        | Sendmsg { conn; fd; iovecs = _ } ->
          if res < 0 then Unix.unix_error (-res) "Io_uring.sendmsg" "";
          Httpaf.Server_connection.report_write_result
            conn
            (if res = 0 then `Closed else `Ok res);
          writer_thread ~conn ~fd
        | Yield_writer { conn; fd } -> writer_thread ~conn ~fd);
    Io_uring.clear_completions io_uring
  done
;;

let () =
  Command.run
  @@ Command.basic ~summary:"toy http server using io_uring"
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
     in
     fun () -> run ~queue_depth ~port ~backlog ()
;;
