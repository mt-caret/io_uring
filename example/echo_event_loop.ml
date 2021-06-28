open Core
open Core.Unix

module Ring = struct
  type t = { ring : (int -> int -> unit) Io_uring.t }

  let make ~ring_size =
    { ring =
        Io_uring.create
          ~max_submission_entries:ring_size
          ~max_completion_entries:(ring_size * 2)
    }
  ;;

  let wait t =
    let _submitted = Io_uring.submit t.ring in
    Stdio.Out_channel.flush Stdio.Out_channel.stdout;
    Io_uring.wait t.ring ~timeout:`Never;
    Io_uring.iter_completions t.ring ~f:(fun ~user_data ~res ~flags ->
        user_data res flags);
    Io_uring.clear_completions t.ring;
    ()
  ;;

  let global = make ~ring_size:1024
end

module Deferred = struct
  type 'a t =
    { begin_ : unit -> unit
    ; mutable then_ : ([ `Ok of 'a | `Eof | `Error ] -> unit) option
    }

  let create creater_fun =
    let rec new_t =
      { begin_ =
          (fun () ->
            creater_fun (fun result ->
                match new_t.then_ with
                | Some v -> v result
                | None -> printf "Warning: Ignored Result\n"))
      ; then_ = None
      }
    in
    new_t
  ;;

  let bind t f =
    t.then_ <- Some f;
    t
  ;;
end

module Scheduler = struct
  type t =
    { ring : Ring.t
    ; worklist : (unit -> unit) Queue.t
    }

  let create () = { ring = Ring.global; worklist = Queue.create () }
  let schedule t (deferred : 'a Deferred.t) = Queue.enqueue t.worklist deferred.begin_
  let poll t = Ring.wait t.ring

  let do_work t =
    Queue.iter t.worklist ~f:(fun x -> x ());
    Queue.clear t.worklist
  ;;

  let rec block_forever t =
    do_work t;
    poll t;
    block_forever t
  ;;

  let global = create ()
end

module Socket = struct
  type t = { fd : File_descr.t }

  let make fd = { fd }

  let accept t =
    Deferred.create (fun result_fn ->
        ignore
          (Io_uring.prepare_accept
             Ring.global.ring
             Io_uring.Sqe_flags.none
             t.fd
             (fun res _flags ->
               let accept_result =
                 if res = -1 then `Error else `Ok (make (File_descr.of_int res))
               in
               result_fn accept_result)
            : Io_uring.Queued_sockaddr.t option)
        (* TODO: Don't ignore *))
  ;;

  let read t ~max_length =
    let buffer = Bigstring.create max_length in
    Deferred.create (fun result_fn ->
        ignore
          (Io_uring.prepare_recv
             Ring.global.ring
             Io_uring.Sqe_flags.none
             t.fd
             buffer
             (fun res _flags ->
               let recv_result =
                 if res = -1 then `Error else if res = 0 then `Eof else `Ok (res, buffer)
               in
               result_fn recv_result)
            : bool)
        (* TODO: Don't ignore *))
  ;;

  let write t ~buffer =
    Deferred.create (fun result_fn ->
        ignore
          (Io_uring.prepare_write
             Ring.global.ring
             Io_uring.Sqe_flags.none
             t.fd
             buffer
             ~offset:0
             (fun res _flags ->
               let write_result =
                 if res = -1 then `Error else if res = 0 then `Eof else `Ok (res, buffer)
               in
               result_fn write_result)
            : bool)
        (* TODO: Don't ignore *))
  ;;
end

let rec reader_writer new_socket =
  Deferred.bind (Socket.read new_socket ~max_length:2048) (fun result ->
      match result with
      | `Ok (bytes_read, buffer) ->
        printf "Read %i bytes\n" bytes_read;
        Scheduler.schedule Scheduler.global (reader_writer new_socket);
        Scheduler.schedule Scheduler.global (Socket.write new_socket ~buffer)
      | `Error -> printf "Error in stream\n"
      | `Eof -> printf "Eof\n")
;;

let run ~port ~backlog =
  let sockfd = socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  setsockopt sockfd SO_REUSEADDR true;
  let addr = ADDR_INET (Unix.Inet_addr.localhost, port) in
  bind sockfd ~addr;
  listen sockfd ~backlog;
  printf "Listening\n";
  let server_socket = Socket.make sockfd in
  printf "Created server socket\n";
  let rec accept_fn () =
    printf "Scheduling an accept\n";
    Scheduler.schedule
      Scheduler.global
      (Deferred.bind (Socket.accept server_socket) (fun new_socket ->
           match new_socket with
           | `Ok new_socket ->
             Scheduler.schedule Scheduler.global (reader_writer new_socket);
             accept_fn ()
           | `Eof | `Error ->
             raise_s [%message "Something really bad happened when accepting"]))
  in
  accept_fn ();
  Scheduler.block_forever Scheduler.global
;;

let () = run ~port:8000 ~backlog:64
