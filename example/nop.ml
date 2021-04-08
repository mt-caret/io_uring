open Core

let () =
  let max_submission_entries = 10_000 in
  let io_uring = Io_uring.create ~max_submission_entries ~max_completion_entries:1024 in
  let start = Time_ns.now () in
  for i = 1 to max_submission_entries do
    match%optional.Io_uring.Tag.Option Io_uring.nop io_uring 0 with
    | None -> failwith "submission queue is full"
    | Some _tag -> ()
  done;
  let n_submitted = Io_uring.submit io_uring in
  Io_uring.wait io_uring ~timeout:`Never;
  let diff = Time_ns.diff (Time_ns.now ()) start in
  Io_uring.close io_uring;
  print_s [%message "" (diff : Time_ns.Span.t) (n_submitted : int)]
;;
