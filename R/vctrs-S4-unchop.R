#' @include vctrs-S4-.R

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
  list_unchop(
    x,
    ptype = vec_ptype_common_list(x, NULL),
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


vec_c <- function(...) {
  dots <- rlang::list2(...)
  list_unchop(x = dots, ptype = vec_ptype_common_list(dots, NULL))
}


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

#' @export
vec_ptype_common <- function(..., .ptype = NULL) {
  vec_ptype_common_list(rlang::list2(...), .ptype = .ptype)
}

vec_ptype_common_list <- function(dots, .ptype) {
  dots <- lapply(dots, vec_slice, 0L)
  if (is.null(.ptype)) {
    base::Reduce(vec_ptype2, x = dots)
  } else {
    base::Reduce(vec_ptyp2, x = dots, init = .ptype)
  }
}
