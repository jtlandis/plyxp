# The following functions should
# be available to users from our package.
#
# We should not have new standard generics made from ours,
# and instead use BiocGenerics

#' @importFrom BiocGenerics rownames colnames nrow ncol
#' @export
BiocGenerics::rownames

#' @export
BiocGenerics::colnames

#' @export
BiocGenerics::nrow

#' @export
BiocGenerics::ncol
