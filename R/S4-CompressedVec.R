
#' @title Compressed Vectors
#' @description
#' Bioconductor's IRanges package supports a variaty of
#' "Compressed*List" variants such as "CompressedIntegerList" or "CompressedCharacterList", etc.
#' Instead of defining a specific class for all subclasses,
#' the "CompressedVectorList" class is the non-Virtual version of
#' "CompressedAtomicList".
#'
#' @keywords internal
setClass("CompressedVectorList", contains = "CompressedAtomicList", prototype = list(elementType = "ANY"))

# @title Compress a Vector
# @description
# The generic constructor of "CompressedVectorList". Behaves similar to vec_chop except that an
# S4 CompressedList object is returned instead of a list.
# @param x a vector
# @param indices a list of integers specifying how to partition x
#
# @returns a CompressedVectorList where the "elementType" slot is class(x)[1]
# @keywords internal
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
