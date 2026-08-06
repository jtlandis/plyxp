#' @include vctrs-S4-.R

#' @title unchop a list of objects
#' @name list_unchop
#' @description
#' A generic version of [`vctrs::list_unchop`][vctrs::list_unchop] meant to
#' support S4 Vectors.
#' @param x a list
#' @param ptype the expected prototype of the output
#' @param ... unused arguments
#' @param indices optional list of integer vectors whose size is equal to that
#' of x. This maps the the final index of each element in the output.
#' @return an object of type `ptype` or the common ptype of elements of x.
#' @export
list_unchop <- new_generic(
  "list_unchop",
  c("x", "ptype"),
  function(x, ptype = NULL, ..., indices = NULL) {
    S7::S7_dispatch()
  }
)


method(
  list_unchop,
  list(x = S7::class_list, ptype = class_vctrs)
) <- function(x, ptype, ..., indices = NULL) {
  vctrs::list_unchop(x, indices = indices, ptype = ptype)
}

method(
  list_unchop,
  list(x = S7::class_list, ptype = NULL)
) <- function(x, ptype, ..., indices = NULL) {
  ptype <- vec_ptype_common_list(x, NULL)
  # prevent recusion case
  if (is.null(ptype)) {
    return(NULL)
  }
  list_unchop(
    x,
    ptype = ptype,
    indices = indices
  )
}

method(
  list_unchop,
  list(x = S7::class_list, ptype = S7::class_any)
) <- function(x, ptype, ..., indices = NULL) {
  vctrs::list_unchop(x, indices = indices, ptype = ptype)
}

method(
  list_unchop,
  list(x = S7::class_list, ptype = class_s4_vctrs)
) <-
  function(x, ptype, ..., indices = NULL) {
    merged <- do.call("c", x)
    if (!is.null(indices)) {
      indices <- vctrs::list_unchop(indices)
      merged <- vec_slice(merged, order(indices))
    }
    merged
  }

method(
  list_unchop,
  list(x = S7::class_list, ptype = class_DF)
) <-
  function(x, ptype, ..., indices = NULL) {
    merged <- do.call("rbind", x)
    if (!is.null(indices)) {
      indices <- vctrs::list_unchop(indices)
      merged <- vec_slice(merged, order(indices))
    }
    merged
  }


vec_c <- function(...) {
  dots <- rlang::list2(...)
  list_unchop(x = dots, ptype = vec_ptype_common_list(dots, NULL))
}

#' Create a common prototype of two vectors
#' @description
#' given two objects, `vec_ptype2()` finds a common prototype
#' @param x first object
#' @param y second object
#' @param ... unused arguments
#' @return an object of size 0.
#' @export
vec_ptype2 <- new_generic(
  "vec_ptype2",
  c("x", "y"),
  function(x, y, ...) S7::S7_dispatch()
)

method(
  vec_ptype2,
  list(
    class_vctrs,
    class_vctrs
  )
) <- function(x, y, ...) {
  vctrs::vec_ptype2(x, y, ...)
}

method(
  vec_ptype2,
  list(
    S7::class_any,
    NULL
  )
) <- function(x, y, ...) {
  if (methods::is(x, "Vector")) {
    vec_slice(x, 0)
  } else {
    vctrs::vec_ptype(x)
  }
}

method(
  vec_ptype2,
  list(
    NULL,
    S7::class_any
  )
) <- function(x, y, ...) {
  if (methods::is(y, "Vector")) {
    vec_slice(y, 0)
  } else {
    vctrs::vec_ptype(y)
  }
}

S7::method(
  vec_ptype2,
  signature = list(x = S7::class_data.frame, y = S7::class_data.frame)
) <- function(x, y, ...) {
  vctrs::vec_ptype2(
    x = x,
    y = y,
    ...,
    x_arg = "x",
    y_arg = "y",
    call = caller_env()
  )
}

plyxp_size_common_list <- function(dots) {
  size <- 1L
  j <- 0L
  iseq <- seq_len(length(dots))
  for (i in iseq) {
    dot <- .subset2(dots, i)
    dot_size <- vec_size(dot)
    if (dot_size != 1L) {
      if (size == 1L) {
        size <- dot_size
        j <- i
      } else if (size != dot_size) {
        stop(
          sprintf(
            "Can't recycle `..%i` (size %i) to match `..%i` (size %i)",
            j,
            size,
            i,
            dot_size
          )
        )
      }
    }
  }
  size
}


plyxp_size_common <- function(...) {
  plyxp_size_common_list(rlang::list2(...))
}

new_DF <- function(
  .data,
  nrows = NULL,
  rownames = NULL
) {
  nrows <- nrows %||% do.call(vec_size_common, .data)
  S4Vectors::new2(
    "DFrame",
    listData = .data,
    nrows = nrows,
    rownames = rownames,
    check = FALSE
  )
}

S7::method(
  vec_ptype2,
  signature = list(x = class_DF, y = S7::class_data.frame)
) <- function(x, y, ...) {
  x <- vec_slice(x, 0L)
  y <- vctrs::vec_ptype(y, x_arg = "y", call = caller_env())
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      vec_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


S7::method(
  vec_ptype2,
  signature = list(x = S7::class_data.frame, y = class_DF)
) <- function(x, y, ...) {
  x <- vctrs::vec_ptype(x, x_arg = "x", call = caller_env())
  y <- vec_slice(y, 0L)
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      vec_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


S7::method(
  vec_ptype2,
  signature = list(x = class_DF, y = class_DF)
) <- function(x, y, ...) {
  x <- vec_slice(x, 0L)
  y <- vec_slice(y, 0L)
  col_names <- union(names(x), names(y))
  names(col_names) <- col_names
  lapply(
    col_names,
    function(name, x, y) {
      vec_ptype2(x[[name]], y[[name]])
    },
    x = x,
    y = y
  ) |>
    new_DF(nrows = 0L)
}


attempt_ptype2 <- function(x, y) {
  out <- c(x, y)
  out[0]
}

method(
  vec_ptype2,
  list(
    class_s4_vctrs,
    class_vctrs
  )
) <- attempt_ptype2

method(
  vec_ptype2,
  list(
    class_vctrs,
    class_s4_vctrs
  )
) <- attempt_ptype2


method(
  vec_ptype2,
  list(
    class_s4_vctrs,
    class_s4_vctrs
  )
) <- attempt_ptype2

#' find the common ptype
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> a collection of objects
#' @param .ptype the expected prototype
#' @return an object of size 0.
#' @export
vec_ptype_common <- function(..., .ptype = NULL) {
  vec_ptype_common_list(rlang::list2(...), .ptype = .ptype)
}

vec_ptype_common_list <- function(dots, .ptype) {
  dots <- lapply(dots, vec_slice, 0L)
  ptype <- base::Reduce(vec_ptype2, x = dots, init = .ptype)
  # if (vctrs::is_partial(ptype)) {
  #   ptype <- vctrs::vec_ptype_finalise(ptype)
  # }
  plyxp_ptype_finalise(ptype)
}

plyxp_ptype_finalise <- new_generic(
  "plyxp_ptype_finalise",
  "x",
  function(x, ...) S7::S7_dispatch()
)

method(plyxp_ptype_finalise, class_vctrs) <- function(x, ...) {
  vctrs::vec_ptype_finalise(x)
}

method(plyxp_ptype_finalise, S7::class_any) <- function(x, ...) {
  x
}
