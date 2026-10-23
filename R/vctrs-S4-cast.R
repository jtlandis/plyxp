#' @include vctrs-S4-.R
#' @include vctrs-S4-init.R

vec_cast <- new_generic("vec_cast", c("x", "to"), function(x, to, ...) {
  S7::S7_dispatch()
})

.s7_class <- S7::new_S3_class(c("S7_class", "S7_object"))

method(vec_cast, list(class_vctrs, class_vctrs)) <- vctrs::vec_cast


method(vec_cast, list(.s7_class, S7::class_any)) <- function(x, to, ...) {
  tocls <- class_name_(obj_class(to))
  rlang::abort(
    message = c(
      sprintf("cannot cast an `S7_class` to %s", tocls),
      "i" = "did you mean to cast the result of the S7_class constructor?"
    )
  )
}

method(vec_cast, list(S7::class_any, S7::class_any)) <- function(x, to, ...) {
  if (identical(xcls <- obj_class(x), tocls <- obj_class(to))) {
    # consider registering identity for this case?
    return(x)
  }
  # if x or to is an S4 class, perhapse we should check
  # selectMethod(coerce, c(xcls@className, tocls@className))?
  xcls <- class_name_(xcls)
  tocls <- class_name_(tocls)
  rlang::abort(
    message = c(
      sprintf("cannot cast %s to %s", xcls, tocls),
      "i" = "consider registering the method:",
      sprintf(
        "S7::method(plyxp::vec_cast, list(%s, %s) <- function(x, to, ...) {...}",
        xcls,
        tocls
      )
    )
  )
}

pre_cast_df <- function(x, to, xsize = vec_size(x)) {
  ton <- length(to)
  xnm <- colnames(x)
  tonm <- colnames(to)
  if (!all(xnm %in% tonm)) {
    stop("cannot lossy cast `x` to `to`")
  }
  out <- vector("list", ton)
  names(out) <- tonm
  for (name in xnm) {
    out[[name]] <- vec_cast(x[[name]], to[[name]])
  }
  tonm <- setdiff(tonm, xnm)
  for (name in tonm) {
    out[[name]] <- vec_init(to[[name]], n = xsize)
  }
  out
}

method(vec_cast, list(class_DF, S7::class_data.frame)) <- function(x, to, ...) {
  xsize <- x@nrows
  out <- pre_cast_df(x, to, xsize = xsize)
  attr(out, "row.names") <- c(NA_integer_, -xsize)
  class(out) <- class(to)
  out
}

method(
  vec_cast,
  list(S7::class_data.frame | class_DF, class_DF)
) <- function(x, to, ...) {
  xsize <- nrow(x)
  out <- pre_cast_df(x, to, xsize = xsize)
  S4Vectors::new2(
    "DFrame",
    listData = out,
    nrows = xsize,
    check = FALSE
  )
}
