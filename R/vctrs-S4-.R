

#' @title S7 classes for vctrs and S4 Vectors
#' @name vectors
#' @description
#' A set of S7 classes and Class unions that help establish S7 method dispatch.
#' These classes were made to re-export several `vctrs` functions such that
#' internals for `plyxp` were consistent with room for optimization.
#' @seealso [vec_rep()],[vec_recycle()],[vec_slice()]
#' @return S7 class union or base class
#' @export
class_vctrs <- S7::new_union(S7::class_atomic, S7::class_list,
                             S7::class_data.frame, S7::class_factor,
                             S7::class_Date, S7::class_POSIXct)

#' @rdname vectors
#' @export
class_s4_vctrs <- methods::getClass("Vector")

#' @rdname vectors
#' @export
class_DF <- methods::getClass("DataFrame")