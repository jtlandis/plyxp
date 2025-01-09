vec_chop2 <- new_generic(
  "vec_chop2",
  dispatch_args = "x",
  function(x, ..., indices = NULL) {
    S7_dispatch()
  }
)

method(vec_chop2, class_vctrs) <- function(
  x,
  ...,
  indices = NULL,
  sizes = NULL
) {
  vctrs::vec_chop(x = x, ..., indices = indices, sizes = sizes)
}

method(vec_chop2, class_s4_vctrs) <- function(x, ..., indices = NULL) {
  fun <- method(vec_slice, object = x)
  if (is.null(indices)) {
    indices <- seq_along(x)
  }
  purrr::map(indices, fun, x = x)
}

#---- unchops

list_unchop2 <- new_generic(
  "list_unchop2",
  dispatch_args = "x",
  function(
    x,
    ...,
    indices = NULL
  ) {
    S7_dispatch()
  }
)

method(list_unchop2, class_vctrs) <- function(
  x,
  ...,
  indices = NULL
) {
  vctrs::list_unchop(
    x = x,
    indices = indices,
    ...
  )
}
