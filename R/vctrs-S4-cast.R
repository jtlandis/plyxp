#' @include vctrs-S4-.R

vec_cast <- new_generic("vec_init", c("x", "to"), function(x, to, ...) {
  S7::S7_dispatch()
})


method(vec_cast, list(class_vctrs, class_vctrs)) <- vctrs::vec_cast

method(vec_cast, list(S7::class_any, S7::class_any)) <- function(x, to, ...) {
  if (identical(obj_class(x), obj_class(to))) {
    return(x)
  }
  xcls <- obj_class_name(x)
  tocls <- obj_class_name(to)
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

method(vec_cast, list(class_DF, S7::class_data.frame)) <- function(x, to, ...) {
  xn <- length(x)
  ton <- length(to)
  if (
    xn > ton ||
      (!is.null(mcols <- mcols(x)) && length(mcols) > 0)
  ) {
    stop("cannot lossy cast `x` to `to`")
  }

  xsize <- vec_size(x)
  new_data <- vec_init(to, xsize)
  listData <- x@listData
  for (name in colnames(x)) {
    new_data[[name]] <- vec_cast(listData[[name]], new_data[[name]])
  }
  attr(new_data, "row.names") <- c(NA_integer_, -nn)
  class(new_data) <- "data.frame"
  new_data
}

method(
  vec_cast,
  list(S7::class_data.frame | class_DF, class_DF)
) <- function(x, to, ...) {
  xn <- length(x)
  ton <- length(to)
  if (xn > ton) {
    stop("cannot lossy cast `x` to `to`")
  }
  xsize <- vec_size(x)
  new_data <- vec_init(to, xsize)
  listData <- to@listData
  for (name in colnames(x)) {
    new_data[[name]] <- vec_cast(x[[name]], listData[[name]])
  }
  new_data
}
