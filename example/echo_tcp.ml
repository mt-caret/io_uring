open Core

let queue_depth = 2048

module User_data = struct
  type t =
    | Accept
    | Recv of Unix.File_descr.t * Bigstring.t
    | Send of Unix.File_descr.t * Bigstring.t * int
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
    | Recv (fd, buf) ->
      let sq_full = Io_uring.prepare_recv io_uring Io_uring.Sqe_flags.none fd buf t in
      if sq_full then raise_s [%message "recv: submission queue is full"]
    | Send (fd, buf, len) ->
      let sq_full =
        Io_uring.prepare_send io_uring Io_uring.Sqe_flags.none fd ~len buf t
      in
      if sq_full then raise_s [%message "send: submission queue is full"]
  ;;
end

let submit io_uring =
  let ret = Io_uring.submit io_uring in
  if ret < 0 then Unix.unix_error (-ret) "Io_uring.submit" ""
;;

let run ~queue_depth ~port ~backlog ~max_message_len =
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
  User_data.(prepare Accept) ~io_uring ~sockfd ~queued_sockaddr_ref;
  let prepare = User_data.prepare ~io_uring ~sockfd ~queued_sockaddr_ref in
  while true do
    submit io_uring;
    Io_uring.wait io_uring ~timeout:`Never;
    let handle_completion ~user_data ~res ~flags =
      match (user_data : User_data.t) with
      | Accept ->
        if res < 0 then Unix.unix_error (-res) "Io_uring.accept" "";
        User_data.Recv (Unix.File_descr.of_int res, Bigstring.create max_message_len)
        |> prepare;
        let sockaddr =
          Option.bind !queued_sockaddr_ref ~f:Io_uring.Queued_sockaddr.thread_unsafe_get
          |> Option.value_exn
        in
        print_s [%message "client connected" (sockaddr : Unix.sockaddr)];
        prepare User_data.(Accept)
      | Recv (fd, buf) ->
        (* TODO: fix handling *)
        if res < 0 then Unix.unix_error (-res) "Io_uring.recv" "";
        User_data.Send (fd, buf, res) |> prepare
      | Send (fd, buf, _len) ->
        if res < 0 then Unix.unix_error (-res) "Io_uring.send" "";
        User_data.Recv (Unix.File_descr.of_int res, buf) |> prepare
    in
    Io_uring.iter_completions io_uring ~f:handle_completion;
    Io_uring.clear_completions io_uring
  done
;;

let () = run ~queue_depth:2048 ~port:8000 ~backlog:100 ~max_message_len:4096
