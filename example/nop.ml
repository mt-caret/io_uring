open Core

let () =
  Command.run
  @@ Command.basic ~summary:"Naive benchmark of nop submssion to io_uring"
  @@ let%map_open.Command max_submission_entries =
       flag "sq-size" (optional_with_default 1024 int) ~doc:"INT size of submission queue"
     and _max_completion_entries =
       flag "cq-size" (optional_with_default 1024 int) ~doc:"INT size of completion queue"
     in
     fun () ->
       let io_uring =
         Io_uring.create ~max_submission_entries ~max_completion_entries:1024
       in
       let start = Time_ns.now () in
       for _i = 1 to max_submission_entries do
         let sq_full = Io_uring.prepare_nop io_uring Io_uring.Sqe_flags.none 0 in
         if sq_full then failwith "submission queue is full"
       done;
       let n_submitted = Io_uring.submit io_uring in
       Io_uring.wait io_uring ~timeout:`Never;
       let diff = Time_ns.diff (Time_ns.now ()) start in
       Io_uring.close io_uring;
       print_s [%message "" (diff : Time_ns.Span.t) (n_submitted : int)]
;;
