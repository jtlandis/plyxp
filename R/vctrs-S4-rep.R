
#' @title replicate a vector
#' @name vctrs-vec_rep
#' @description
#' A re-export of [`vctrs::vec_rep`][vctrs::vec_rep] and 
#' [`vctrs::vec_rep_each`][vctrs::vec_rep_each] as an S7 generic
#' function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_rep
#' @return a new S3 or S4 vector replicated by specified times
#' @examples
#' vec_rep(1:2, times = 5)
#' vec_rep(S4Vectors::Rle(1:2), times = 5)
#' 
#' vec_rep_each(1:2, times = 5)
#' vec_rep_each(S4Vectors::Rle(1:2), times = 5)
#' @export
vec_rep <- new_generic("vec_rep", dispatch_args = "x",
                       function(x, times, ..., 
                                error_call = caller_env(), x_arg = "x",
                                times_arg = "times") {
                             S7_dispatch()
                           })

method(vec_rep, class_vctrs) <- function(x, times, ..., 
                                             error_call = caller_env(), x_arg = "x",
                                             times_arg = "times") {
  vctrs::vec_rep(x = x, times = times, ..., x_arg = x_arg, times_arg = times_arg, error_call = error_call)
}

method(vec_rep, class_s4_vctrs) <- function(x, times, ..., 
                                            error_call = caller_env(),
                                            x_arg = "x",
                                            times_arg = "times") {

  times <- vctrs::vec_cast(times, integer(1), x_arg = times_arg)
  if (length(length) != 1L) abort("`times` must be a single number")
  rep(x, times)
}

#' @rdname vctrs-vec_rep
#' @export
vec_rep_each <- new_generic("vec_rep_each", dispatch_args = "x",
                       function(x, times, ..., 
                                error_call = caller_env(), x_arg = "x",
                                times_arg = "times") {
                         S7_dispatch()
                       })

method(vec_rep_each, class_vctrs) <- function(x, times, ..., 
                                         error_call = caller_env(),
                                         x_arg = "x",
                                         times_arg = "times") {
  vctrs::vec_rep_each(x = x, times = times, ..., x_arg = x_arg,
                      times_arg = times_arg, error_call = error_call)
}

method(vec_rep_each, class_s4_vctrs) <- function(x, times, ..., 
                                                error_call = caller_env(),
                                                x_arg = "x",
                                                times_arg = "times") {
  
  times <- vctrs::vec_cast(times, integer(1), x_arg = times_arg)
  if (length(length) != 1L) abort("`times` must be a single number")
  rep(x, each = times)
}