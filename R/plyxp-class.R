#' @name new_plyxp
#' @title SummarizedExperiment Shell Object
#' @description
#' A container object for the SummarizedExperiment class.
#' @param se SummarizedExperiment object
#' @return PlySummarizedExperiment object
#' @examples
#' plyxp(se = se_simple)
#' @export
new_plyxp <- function(se) {
  PlySummarizedExperiment(se = se)
}

#' @rdname new_plyxp
#' @description
#' This is an S7 class only exported for possible extension.
#' @export
PlySummarizedExperiment <-
  S7::new_class(
    "PlySummarizedExperiment",
    properties = list(
      se = S7::new_property(
        class = methods::getClass("SummarizedExperiment")
      )
    )
  )

#' @export
plyxp <- function(.data, .f, ...) {
  .f <- rlang::as_function(.f)
  out <- .f(.data@se, ...)
  if (!methods::is(out, "SummarizedExperiment")) {
    cli::cli_abort("{.arg .f} must return a {.cls SummarizedExperiment} object")
  }
  .data@se <- out
  .data
}

#' @export
print.PlySummarizedExperiment <- function(x, ...) {
  show_tidy(x@se, ...)
}
