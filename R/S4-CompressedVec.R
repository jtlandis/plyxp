
#' @export
setClass("CompressedVectorList", contains = "CompressedAtomicList", prototype = list(elementType = "ANY"))

#' @export
compress <- function(x, indices = NULL) {
  if (is.null(indices)) {
    part <- IRanges::PartitioningByEnd(seq_len(vec_size(x)))
  } else {
    stopifnot(
      "indices should be NULL or a list"=is.list(indices),
      "indices should be a list of integers"=all(vapply(indices, inherits, TRUE, what = "integer"))
    )
    x <- vec_slice(x, vctrs::list_unchop(indices))
    part <- IRanges::PartitioningByEnd(cumsum(lengths(indices)))
  }
  S4Vectors::new2("CompressedVectorList",
    unlistData = x,
    elementType = class(x)[1],
    elementMetadata = NULL,
    partitioning = part,
    check = FALSE
  )
}
