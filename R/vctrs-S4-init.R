#' @include vctrs-S4-.R

vec_init <- new_generic("vec_init", "x", function(x, n = 1L) {
  n <- as.integer(n)
  S7::S7_dispatch()
})


method(vec_init, class_vctrs) <- vctrs::vec_init

method(vec_init, class_DF) <- function(x, n = 1L) {
  listData <- lapply(x@listData, vec_init, n = n)
  mcols <- x@elementMetadata
  if (!is.null(mcols) && mcols@nrows != length(x)) {
    stop("cannot init DFrame. Improper mcols length")
  }
  S4Vectors::new2(
    "DFrame",
    listData = listData,
    nrows = n,
    elementMetadata = mcols,
    check = FALSE
  )
}
