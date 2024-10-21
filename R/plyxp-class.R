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

#' @export
`[.PlySummarizedExperiment` <- function(x, i, j, ..., drop = TRUE) {
  type <- NULL
  if (!missing(i)) {
    type <- "i"
  }
  if (!missing(j)) {
    type <- paste0(type, "j")
  }
  x@se <- switch(type,
    i = x@se[i, ],
    j = x@se[, j],
    ij = x@se[i, j],
    x@se
  )
  x
}

#' @export
`$.PlySummarizedExperiment` <- function(x, name) {
  colData(x@se)[[name]]
}

#' @export
dim.PlySummarizedExperiment <- function(x) {
  dim(x@se)
}

S7::S4_register(PlySummarizedExperiment)

setMethod(
  "assays", "PlySummarizedExperiment",
  function(x, withDimnames = TRUE, ...) {
    assays(x@se, withDimnames = withDimnames, ...)
  }
)

set_assays <- function(
    x, withDimnames = TRUE, ..., value) {
  plyxp(x, `assays<-`, value = value, withDimnames = withDimnames, ...)
}

# S7_signature <- function(...) {
#   out <- list(...)
#   class(out) <- "S7_signature"
#   out
# }

setMethod(
  "assays<-", c("PlySummarizedExperiment", "list"),
  set_assays
)

setMethod(
  "assays<-", c("PlySummarizedExperiment", "SimpleList"),
  set_assays
)

get_assay <- function(x, i, withDimnames = TRUE, ...) {
  assay(x@se, i = i, withDimnames = withDimnames, ...)
}

setMethod(
  "assay", c("PlySummarizedExperiment", "missing"),
  function(x, i, withDimnames = TRUE, ...) {
    assay(x@se, withDimnames = withDimnames, ...)
  }
)
setMethod("assay", c("PlySummarizedExperiment", "numeric"), get_assay)
setMethod("assay", c("PlySummarizedExperiment", "character"), get_assay)

set_assay <- function(x, i, withDimnames = TRUE, ..., value) {
  plyxp(x, `assay<-`, i = i, withDimnames = withDimnames, ..., value = value)
}

setMethod(
  "assay<-", c("PlySummarizedExperiment", "missing"),
  function(x, i, withDimnames = TRUE, ..., value = value) {
    plyxp(x, `assay<-`, withDimnames = withDimnames, ..., value = value)
  }
)

setMethod("assay<-", c("PlySummarizedExperiment", "numeric"), set_assay)
setMethod("assay<-", c("PlySummarizedExperiment", "character"), set_assay)


setMethod(
  "rowData", "PlySummarizedExperiment",
  function(
      x,
      use.names = TRUE, ...) {
    rowData(x@se, use.names = use.names, ...)
  }
)

setMethod(
  "rowData<-", "PlySummarizedExperiment",
  function(
      x, ..., value) {
    plyxp(x, `rowData<-`, ..., value = value)
  }
)

setMethod(
  "colData", "PlySummarizedExperiment",
  function(x, ...) {
    colData(x@se, ...)
  }
)

setMethod(
  "colData<-",
  c("PlySummarizedExperiment", "DataFrame"),
  function(x, ..., value) {
    plyxp(x, `colData<-`, ..., value = value)
  }
)

setMethod(
  "colData<-",
  c("PlySummarizedExperiment", "NULL"),
  function(x, ..., value) {
    plyxp(x, `colData<-`, ..., value = value)
  }
)
