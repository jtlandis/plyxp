#' @name plyxp
#' @title SummarizedExperiment Shell Object
#' @description
#' A container object for the SummarizedExperiment class.
#' @param se SummarizedExperiment object
#' @return PlySummarizedExperiment object
#' @examples
#' plyxp(se = se_simple)
#' @export
plyxp <- function(se) {
  PlySummarizedExperiment(se = se)
}

#' @rdname plyxp
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
