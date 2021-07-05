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
- [x] test `prep_send`/`prep_recv` with httpaf?
- [x] check for errno in library
- [x] add ``Wait_for_num_completions of int` to `Io_uring.wait`
- [x] allocate Queued_sockaddr on the heap
- [ ] add buffer provision support (and add it in echo server)
- [ ] keep track of in-flight requests and apply backpressure so completion queue never overflows?
  - backpressure is interesting. Even though we can straightforwardly keep
    track of in-flight requests and make sure we never submit more than the
    number of free slots in the completion queue, this may result in starvation
    when we submit many requests which may have unbounded completion times
    (like accept or poll). We probably need to think about
    [IORING_SETUP_CQ_NODROP](https://lore.kernel.org/io-uring/20191106235307.32196-1-axboe@kernel.dk/T/)
    and clearly document behavior around this.
- [x] Use [the foreign build sandboxing approach](https://dune.readthedocs.io/en/stable/foreign-code.html#foreign-build-sandboxing) in dune to fix the liburing version?
