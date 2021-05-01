open Core

include struct
  module Config = Httpaf.Config
  module Server_connection = Httpaf.Server_connection
end

module User_data = struct
  type t =
    | Recv
    | Writev
  [@@deriving sexp]

  let to_string = Fn.compose Sexp.to_string [%sexp_of: t]
end

module Buffer = struct
  type t =
    { buffer : Bigstring.t
    ; mutable off : int
    ; mutable len : int
    }

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

  let read t ~f = f t.buffer ~off:t.off ~len:t.len

  let finished_reading t n =
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0
  ;;

  let write t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len)
  ;;

  let finished_writing t n = t.len <- t.len + n
end

let to_core_iovec_array (iovecs : Bigstring.t Faraday.iovec list) =
  List.map iovecs ~f:(fun { buffer; off; len } ->
      Unix.IOVec.of_bigstring ~pos:off ~len buffer)
  |> List.to_array
;;

let submit io_uring =
  let ret = Io_uring.submit io_uring in
  if ret < 0 then Unix.unix_error (-ret) "Io_uring.submit" ""
;;

module Server : sig
  val create_connection_handler
    :  ?config:Config.t
    -> request_handler:('a -> Server_connection.request_handler)
    -> error_handler:('a -> Server_connection.error_handler)
    -> 'a
    -> User_data.t Io_uring.t
    -> Unix.File_descr.t
    -> unit
end = struct
  let create_connection_handler
      ?(config = Config.default)
      ~request_handler
      ~error_handler
      client_addr
      io_uring
      sockfd
    =
    let request_handler = request_handler client_addr in
    let error_handler = error_handler client_addr in
    let conn = Server_connection.create ~config ~error_handler request_handler in
    (*let buf = Core.Iobuf.create config.read_buffer_size in*)
    let buf = Buffer.create config.read_buffer_size in
    let read_complete = ref false in
    let write_complete = ref false in
    let read_yielded = ref true in
    let write_yielded = ref true in
    let in_flight = ref 0 in
    while (not !read_complete) || (not !write_complete) || !in_flight > 0 do
      if !read_yielded
      then (
        (match Server_connection.next_read_operation conn with
        | `Read ->
          let sq_full =
            Buffer.read buf ~f:(fun bigstring ~off ~len ->
                Io_uring.prepare_recv
                  io_uring
                  Io_uring.Sqe_flags.none
                  sockfd
                  ~pos:off
                  ~len
                  bigstring
                  User_data.Recv)
          in
          if sq_full then raise_s [%message "sq is full"];
          incr in_flight
        | `Yield -> Server_connection.yield_reader conn (fun () -> read_yielded := true)
        | `Close ->
          (* TODO: close fd if not closed already? *)
          read_complete := true);
        read_yielded := false);
      if !write_yielded
      then (
        match Server_connection.next_write_operation conn with
        | `Write iovecs ->
          let iovecs = to_core_iovec_array iovecs in
          let sq_full =
            Io_uring.prepare_writev
              io_uring
              Io_uring.Sqe_flags.none
              sockfd
              iovecs
              ~offset:(-1)
              User_data.Writev
          in
          if sq_full then raise_s [%message "sq is full"];
          incr in_flight
        | `Yield -> Server_connection.yield_writer conn (fun () -> write_yielded := true)
        | `Close _n ->
          (* TODO: close fd if not closed already? *)
          write_complete := true);
      if !in_flight > 0
      then (
        submit io_uring;
        Io_uring.wait io_uring ~timeout:`Never;
        Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
            match user_data with
            | Recv ->
              if res < 0
              then
                Unix.unix_error (-res) "Io_uring.recv" [%string "%{user_data#User_data}"]
            (*if res = 0 then Server_connection.read_eof*)
            | Writev ->
              if res < 0
                 && not
                      Unix.Error.(
                        [%compare.equal: Unix.Error.t] (of_system_int ~errno:(-res)) EPIPE)
              then
                Unix.unix_error
                  (-res)
                  "Io_uring.writev"
                  [%string "%{user_data#User_data}"];
              Server_connection.report_write_result
                conn
                (if res < 0 then `Closed else `Ok res));
        Io_uring.clear_completions io_uring)
    done
  ;;
end

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of sitting by \
   her sister on the bank, and of having nothing to do: once or twice she had peeped \
   into the book her sister was reading, but it had no pictures or conversations in it, \
   <and what is the use of a book,> thought Alice <without pictures or conversations?> \
   So she was considering in her own mind (as well as she could, for the hot day made \
   her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would \
   be worth the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; \
   nor did Alice think it so very much out of the way to hear the Rabbit say to itself, \
   <Oh dear! Oh dear! I shall be late!> (when she thought it over afterwards, it \
   occurred to her that she ought to have wondered at this, but at the time it all \
   seemed quite natural); but when the Rabbit actually took a watch out of its \
   waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, \
   for it flashed across her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran \
   across the field after it, and fortunately was just in time to see it pop down a \
   large rabbit-hole under the hedge. In another moment down went Alice after it, never \
   once considering how in the world she was to get out again. The rabbit-hole went \
   straight on like a tunnel for some way, and then dipped suddenly down, so suddenly \
   that Alice had not a moment to think about stopping herself before she found herself \
   falling down a very deep well. Either the well was very deep, or she fell very \
   slowly, for she had plenty of time as she went down to look about her and to wonder \
   what was going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the sides of \
   the well, and noticed that they were filled with cupboards......"
  |> Bigstring.of_string
;;

open Httpaf

let request_handler _socket_addr reqd =
  match Reqd.request reqd with
  | { Request.meth = `GET; headers; _ } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x
      in
      Response.create
        ~headers:(Headers.of_list [ "content-type", content_type; "connection", "close" ])
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

let error_handler _socket_addr ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n"
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body
;;

let server = Server.create_connection_handler ~request_handler ~error_handler

let run ~port ~backlog =
  let sockfd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.setsockopt sockfd SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
  Unix.bind sockfd ~addr;
  Unix.listen sockfd ~backlog
;;
