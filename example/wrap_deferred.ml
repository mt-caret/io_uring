open Core
open Async_kernel
open Core.Unix

module Ring = struct
  type t = { ring : (int -> int -> unit) Io_uring.t }

  let make ~ring_size =
    { ring = Io_uring.create
      ~max_submission_entries:ring_size
      ~max_completion_entries:(ring_size * 2) }
  ;;

  let wait t =
    printf "Submit %i\n" (Io_uring.submit t.ring);
    Io_uring.wait t.ring ~timeout:`Never;
    printf "Done wait\n";
    Io_uring.iter_completions t.ring ~f:(fun ~user_data ~res ~flags ->
        printf "Completion\n";
        user_data res flags);
    Io_uring.clear_completions t.ring;
    ()
  ;;
end

module Socket = struct

  type t =
    { ring : Ring.t
    ; fd : File_descr.t
    }

  let make ring fd = { ring; fd = fd }

  let accept t =
    printf "Accept\n";
    Deferred.create (fun result ->
        ignore(Io_uring.prepare_accept
          t.ring.ring
          Io_uring.Sqe_flags.none
          t.fd
          (fun res _flags ->
            let accept_result = if res = -1 then `Eof else `Ok (File_descr.of_int res) in
            Ivar.fill result accept_result;) : Io_uring.Queued_sockaddr.t option) (* TODO: Don't ignore *)
      )
  ;;

  let read t ~max_length =
    let buffer = Bigstring.create max_length in
    Deferred.create (fun result ->
      ignore (Io_uring.prepare_recv
      t.ring.ring
      Io_uring.Sqe_flags.none
      t.fd
      buffer
      (fun res _flags -> (
        let recv_result = if res = -1 then `Error else if res = 0 then `Eof else `Ok (res, buffer) in
        Ivar.fill result recv_result;
      )) : bool) (* TODO: Don't ignore *)
    )
  ;;

  let write t ~buffer =
    Deferred.create (fun result ->
      ignore (Io_uring.prepare_write
      t.ring.ring
      Io_uring.Sqe_flags.none
      t.fd
      buffer
      ~offset:0
      (fun res _flags -> (
        let write_result = if res = -1 then `Error else if res = 0 then `Eof else `Ok (res, buffer) in
        Ivar.fill result write_result;
      )) : bool) (* TODO: Don't ignore *)
    )
  ;;

end

let run ~queue_depth:_ ~port ~backlog ~max_message_len:_ =
  let sockfd = socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  setsockopt sockfd SO_REUSEADDR true;
  let addr = ADDR_INET (Unix.Inet_addr.localhost, port) in
  bind sockfd ~addr;
  listen sockfd ~backlog;
  printf "Listening\n";
  let ring = Ring.make ~ring_size:1024 in
  printf "Created ring\n";
  let server_socket = Socket.make ring sockfd in
  printf "Created server socket\n";
  let rec accept_fn () =
    let%bind _new_socket = Socket.accept server_socket in
    accept_fn ()
  in
  ignore (accept_fn () : 'a Deferred.t);
  printf "Listening\n";
  while true do
    printf "Waiting\n";
    Stdio.Out_channel.flush Stdio.Out_channel.stdout;
    Ring.wait ring;
  done
;;

let () =
  Command.run
  @@ Command.basic ~summary:"echo server using io_uring"
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
     and max_message_len =
       flag
         "max-message-len"
         (optional_with_default 4096 int)
         ~doc:"INT maximum size of messages"
     in
     fun () -> run ~queue_depth ~port ~backlog ~max_message_len
;;
