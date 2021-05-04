OCaml bindings to liburing

much fast, very wip

TODO

- [x] prepend function names with `prepare_` (to prevent name collisions when adding `io_uring_prep_close`)
- [x] port more examples from liburing
- [x] add option to use writev/readv in cp example
- [x] add support for request linking (and add to cp example)
  - note: at first, link-cp.c seems broken since there doesn't seem to be a
    way to read+write the reminder when one of the two syscalls read/writes less
    then expected, but seems like checking for failure and re-submitting works?
    (c.f. https://github.com/axboe/liburing/issues/58)
- [ ] add support for submission queue polling
- [ ] add support for fixed buffers
- [ ] add benchmarks
- [ ] functorize interface to implement a version of the API that explicitly
      checks whether the kernel supports the relevant functions
      (similar to Bigstring_unix.recvmmsg_assume_fd_is_nonblocking)
  - `io_uring_get_probe()`
  - change `'a t ` to `('a, 'witness) t` to prevent cross-instance usage?
    - idea: ensure only one version exists with something like `Io_uring.Make()`
- [ ] add tests
- [ ] create default value with `Obj.magic` and check for emptiness in `user_data` array via `phys_equal` in invariant
- [ ] properly allocate `io_uring`
- [ ] test `prep_send`/`prep_recv` with httpaf?
- [x] check for errno in library
- [x] add ``Wait_for_num_completions of int` to `Io_uring.wait`
- [x] allocate Queued_sockaddr on the heap
- [ ] add buffer provision support (and add it in echo server)
