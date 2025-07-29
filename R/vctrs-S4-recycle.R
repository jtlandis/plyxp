
#' @title Recycle a vector
#' @name vctrs-vec_recycle
#' @description
#' A re-export of [`vctrs::vec_recycle`][vctrs::vec_recycle] as an S7 generic
#' function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_recycle
#' @return a S3 or S4 vector
#' @examples
#' vec_recycle(1L, size = 5L)
#' vec_recycle(S4Vectors::Rle(1L), size = 5L)
#' 
#' @export
vec_recycle <- new_generic("vec_recycle", dispatch_args = "x",
                           function(x, size, ..., x_arg = "",
                                    call = caller_env()) {
                             S7_dispatch()
                           })

method(vec_recycle, class_vctrs) <- function(x, size, ..., x_arg = "",
                                             call = caller_env()) {
  vctrs::vec_recycle(x = x, size = size, ..., x_arg = x_arg, call = call)
}

method(vec_recycle, class_s4_vctrs) <- function(x, size, ..., x_arg = "",
                                                call = caller_env()) {
  
  if (length(size) != 1L) rlang::abort("argument `size` should be length 1L")
  if (!is.numeric(size)) rlang::abort("argument `size` should be integer-ish")
  vec_len <- length(x)
  vec_len |>
    match(c(1L, size), nomatch = 0L) |>
    as.character() |>
    switch(
      `1` = vec_slice(x, vctrs::vec_rep(1L, size)),
      `2` = x,
      rlang::abort(glue::glue("Can't recycle inpute of size {vec_len} to size {size}."),
            call = call)
    )
}