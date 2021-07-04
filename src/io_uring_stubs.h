#include "config.h"
#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <poll.h>
#ifndef ENOATTR
# define ENOATTR ENODATA
#endif
#include <arpa/inet.h>
#include <assert.h>
#include <sys/un.h>

// TOIMPL: move this to jst-config?
#include <liburing.h>

#include "ocaml_utils.h"
#include "unix_utils.h"
#include "socketaddr.h"

/* Bytes_val is only available from 4.06 */
#ifndef Bytes_val
#define Bytes_val String_val
#endif

#ifndef Val_some
static value Val_some(value mlvalue) {
    CAMLparam1(mlvalue);
    CAMLlocal1(aout);

    aout = caml_alloc(1, 0);
    Store_field(aout, 0, mlvalue);

    CAMLreturn(aout);
}
#endif

#ifndef Val_none
#define Val_none Val_int(0)
#endif

#ifndef Some_val
#define Some_val(v) Field(v, 0)
#endif

#ifdef DEBUG
#define debug(fmt, ...) \
            do { fprintf(stderr, fmt, __VA_ARGS__); } while (0)
#else
#define debug(fmt, ...) \
            do {} while (0)
#endif

/** Core io_uring methods **/

#define POLL_FLAG(FLAG) DEFINE_INT63_CONSTANT (poll_##FLAG##_flag, FLAG)

POLL_FLAG(POLLIN)
POLL_FLAG(POLLOUT)
POLL_FLAG(POLLPRI)
POLL_FLAG(POLLERR)
POLL_FLAG(POLLHUP)

#define SQE_FLAG(FLAG) DEFINE_INT63_CONSTANT (sqe_##FLAG##_flag, FLAG)

SQE_FLAG(IOSQE_FIXED_FILE)
SQE_FLAG(IOSQE_IO_DRAIN)
SQE_FLAG(IOSQE_IO_LINK)
SQE_FLAG(IOSQE_IO_HARDLINK)
SQE_FLAG(IOSQE_ASYNC)
SQE_FLAG(IOSQE_BUFFER_SELECT)

#define Io_uring_val(v) (*((struct io_uring **) Data_abstract_val(v)))
#define Io_uring_cqe_val(v) ((struct io_uring_cqe *) Data_abstract_val(v))
