open Core

(* a port of https://github.com/axboe/liburing/blob/master/examples/io_uring-cp.c
 * and https://github.com/axboe/liburing/blob/master/examples/link-cp.c *)

let get_file_size fd =
  let stats = Unix.fstat fd in
  match stats.st_kind with
  | S_REG -> Int64.to_int_exn stats.st_size
  | kind ->
    (* the original implementation calls ioctl(fd, BLKGETSIZE64, &bytes) to get
     * the size when S_BLK, but core and friends don't have a binding for ioctl
     * so not implemented. *)
    raise_s [%message "file kind not supported" (kind : Unix.file_kind)]
;;

let queue_depth = 64
let batch_size = 32 * 1024

module User_data = struct
  type t =
    { kind : [ `Read | `Write ]
    ; buf : (Bigstring.t[@sexp.opaque])
    ; pos : int
    ; offset : int
    ; len : int
    }
  [@@deriving sexp, fields]

  let to_string = Fn.compose Sexp.to_string [%sexp_of: t]

  let dispatch = function
    | `Read -> Io_uring.prepare_readv
    | `Write -> Io_uring.prepare_writev
  ;;

  let dispatch_non_v = function
    | `Read -> Io_uring.prepare_read
    | `Write -> Io_uring.prepare_write
  ;;

  (* the original implementation uses readv/writev, but we demonstrate use of
    * read/write here as well. *)
  let prepare ?(use_non_v = false) t ~io_uring ~sqe_flags ~fd =
    let { kind; buf; pos; offset; len } = t in
    let sq_full =
      if use_non_v
      then
        dispatch_non_v
          kind
          io_uring
          sqe_flags
          fd
          ~pos
          ~len:(len - pos)
          buf
          ~offset:(offset + pos)
          t
      else (
        let iovec = Unix.IOVec.of_bigstring ~pos ~len:(len - pos) buf in
        dispatch kind io_uring sqe_flags fd [| iovec |] ~offset:(offset + pos) t)
    in
    if sq_full then raise_s [%message "submission queue is full"]
  ;;

  let create = Fields.create
end

let submit io_uring =
  let ret = Io_uring.submit io_uring in
  if ret < 0 then Unix.unix_error (-ret) "Io_uring.submit" ""
;;

let copy_file ~io_uring ~infd ~outfd ~insize ~debug ~use_non_v =
  let sqe_flags = Io_uring.Sqe_flags.none in
  let offset = ref 0 in
  let bytes_to_issue_read = ref insize in
  let bytes_to_issue_write = ref insize in
  let read_submissions = ref 0 in
  let write_submissions = ref 0 in
  while !bytes_to_issue_read > 0 || !bytes_to_issue_write > 0 do
    let submitted = ref false in
    while
      !bytes_to_issue_read > 0 && !read_submissions + !write_submissions < queue_depth
    do
      let len = Int.min !bytes_to_issue_read batch_size in
      let buf = Bigstring.create len in
      User_data.create ~kind:`Read ~buf ~pos:0 ~offset:!offset ~len
      |> User_data.prepare ~use_non_v ~io_uring ~sqe_flags ~fd:infd;
      bytes_to_issue_read := !bytes_to_issue_read - len;
      offset := !offset + len;
      incr read_submissions;
      submitted := true
    done;
    if !submitted then submit io_uring;
    (* Queue is full at this point. Find at least one completion *)
    let first_completion = ref true in
    let got_completion = ref true in
    while !bytes_to_issue_write > 0 && !got_completion do
      Io_uring.wait io_uring ~timeout:(if !first_completion then `Never else `Immediately);
      Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
          if res < 0
          then Unix.unix_error (-res) "Io_uring.read" [%string "%{user_data#User_data}"];
          match user_data.kind with
          | `Read ->
            if user_data.pos + res < user_data.len
            then
              User_data.prepare
                ~use_non_v
                { user_data with pos = user_data.pos + res }
                ~io_uring
                ~sqe_flags
                ~fd:infd
            else (
              User_data.prepare
                ~use_non_v
                { user_data with kind = `Write; pos = 0 }
                ~io_uring
                ~sqe_flags
                ~fd:outfd;
              bytes_to_issue_write := !bytes_to_issue_write - user_data.len;
              decr read_submissions;
              incr write_submissions)
          | `Write ->
            if user_data.pos + res < user_data.len
            then
              User_data.prepare
                ~use_non_v
                { user_data with pos = user_data.pos + res }
                ~io_uring
                ~sqe_flags
                ~fd:outfd
            else decr write_submissions);
      first_completion := false;
      got_completion := Io_uring.num_completions io_uring > 0;
      if !got_completion
      then (
        Io_uring.clear_completions io_uring;
        submit io_uring)
    done
  done;
  (* wait out pending writes *)
  while !write_submissions > 0 do
    Io_uring.wait io_uring ~timeout:`Never;
    Io_uring.iter_completions io_uring ~f:(fun ~user_data ~res ~flags ->
        if res < 0
        then Unix.unix_error (-res) "Io_uring.read" [%string "%{user_data#User_data}"];
        match user_data.kind with
        | `Read -> raise_s [%message "unexpected completion" (user_data : User_data.t)]
        | `Write ->
          (* TODO: the original implementation doesn't check for partial writes
           * here, not sure if on purpose or a bug on their part *)
          if res < user_data.len
          then
            User_data.prepare
              ~use_non_v
              { user_data with pos = user_data.pos + res }
              ~io_uring
              ~sqe_flags
              ~fd:outfd
          else decr write_submissions);
    Io_uring.clear_completions io_uring
  done
;;

(* TODO: kind of flaky and sometimes seems to hang, investigate *)
let copy_file_with_linking ~io_uring ~infd ~outfd ~insize ~debug ~use_non_v =
  let prepare_linked_read_write_pair ~buf ~offset ~len =
    User_data.create ~kind:`Read ~buf ~pos:0 ~offset ~len
    |> User_data.prepare
         ~use_non_v
         ~io_uring
         ~sqe_flags:Io_uring.Sqe_flags.io_link
         ~fd:infd;
    User_data.create ~kind:`Write ~buf ~pos:0 ~offset ~len
    |> User_data.prepare ~use_non_v ~io_uring ~sqe_flags:Io_uring.Sqe_flags.none ~fd:outfd
  in
  let offset = ref 0 in
  let bytes_to_issue = ref insize in
  let submissions = ref 0 in
  let handle_submission ~user_data ~res ~flags =
    if res = -125 (* ECANCELED *)
    then (
      if debug
      then
        print_s
          [%message
            "cancelled, re-submitting"
              (Time_ns.now () : Time_ns.t)
              (user_data : User_data.t)
              (!submissions : int)
              (!bytes_to_issue : int)];
      let { User_data.buf; offset; len; kind = _ } = user_data in
      prepare_linked_read_write_pair ~buf ~offset ~len;
      submissions := !submissions + 2)
    else if res < 0
    then
      Unix.unix_error
        (-res)
        "copy_file_with_linking"
        [%string "%{user_data#User_data} (%{res#Int})"];
    decr submissions
  in
  while !bytes_to_issue > 0 do
    if debug
    then
      print_s
        [%message
          "in initial loop"
            (Time_ns.now () : Time_ns.t)
            (!submissions : int)
            (!bytes_to_issue : int)];
    let submitted = ref false in
    while !bytes_to_issue > 0 && !submissions < queue_depth do
      let len = Int.min !bytes_to_issue batch_size in
      let buf = Bigstring.create len in
      prepare_linked_read_write_pair ~buf ~offset:!offset ~len;
      offset := !offset + len;
      bytes_to_issue := !bytes_to_issue - len;
      submissions := !submissions + 2;
      submitted := true
    done;
    if !submitted then submit io_uring;
    let depth = if !bytes_to_issue > 0 then queue_depth else 1 in
    while !submissions >= depth do
      Io_uring.wait io_uring ~timeout:`Immediately;
      Io_uring.iter_completions io_uring ~f:handle_submission;
      Io_uring.clear_completions io_uring
    done
  done;
  (* TODO: the original implementation omits this part, not sure if on purpose
   * or a bug on their part *)
  while !submissions > 0 do
    if debug
    then
      print_s
        [%message
          "in latter loop"
            (Time_ns.now () : Time_ns.t)
            (!submissions : int)
            (!bytes_to_issue : int)];
    Io_uring.wait io_uring ~timeout:`Never;
    Io_uring.iter_completions io_uring ~f:handle_submission;
    Io_uring.clear_completions io_uring
  done
;;

let () =
  Command.run
  @@ Command.basic ~summary:"cp clone using io_uring"
  @@ let%map_open.Command infile = anon ("infile" %: Filename.arg_type)
     and outfile = anon ("outfile" %: Filename.arg_type)
     and debug = flag "debug" no_arg ~doc:"BOOL enable debug output"
     and use_non_v =
       flag "use-non-v" no_arg ~doc:"BOOL use read/write instead of readv/writev"
     and with_linking = flag "with-linking" no_arg ~doc:"BOOL use io_uring io linking" in
     fun () ->
       let infd = Unix.openfile ~mode:Unix.[ O_RDONLY ] infile in
       let outfd =
         Unix.openfile ~perm:0o644 ~mode:Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] outfile
       in
       let io_uring =
         Io_uring.create ~max_submission_entries:queue_depth ~max_completion_entries:128
       in
       let insize = get_file_size infd in
       if debug
       then
         print_s
           [%message
             "" (infd : Unix.File_descr.t) (outfd : Unix.File_descr.t) (insize : int)];
       (if with_linking then copy_file_with_linking else copy_file)
         ~io_uring
         ~infd
         ~outfd
         ~insize
         ~debug
         ~use_non_v;
       Unix.close infd;
       Unix.close outfd;
       Io_uring.close io_uring
;;
