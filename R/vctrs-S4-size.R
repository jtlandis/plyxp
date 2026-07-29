#' @include vctrs-S4-.R

#' @title replicate a vector
#' @name vctrs-vec_size
#' @description
#' A re-export of [`vctrs::vec_size`][vctrs::vec_size] and
#' as an S7 generic function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_size
#' @return size of the S3 or S4 vector
#' @examples
#' vec_size(1:2, times = 5)
#' vec_size(S4Vectors::Rle(1:2), times = 5)
#' @export
vec_size <- new_generic("vec_size", dispatch_args = "x", function(x) {
  S7_dispatch()
})

method(vec_size, class_vctrs) <- function(x) {
  vctrs::vec_size(x = x)
}

method(vec_size, class_s4_vctrs) <- function(x) {
  length(x)
}

method(vec_size, class_DF) <- function(x) {
  x@nrows
}
