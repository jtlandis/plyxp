#' @name new_plyxp
#' @title SummarizedExperiment Shell Object
#' @description
#' A container object for the SummarizedExperiment class.
#' @param se SummarizedExperiment object
#' @return PlySummarizedExperiment object
#' @examples
#' new_plyxp(se = se_simple@se)
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

#' @name plyxp
#' @title Modify SummarizedExperiment Object
#' @description
#' Modify the underlying SummarizedExperiment object with a function.
#' @param .data a PlySummarizedExperiment object
#' @param .f a function that returns a SummarizedExperiment object
#' @param ... additional arguments passed to `.f`
#' @return a PlySummarizedExperiment object
#' @examples
#' plyxp(se_simple, function(x) x)
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
`$<-.PlySummarizedExperiment` <- function(x, name, value) {
  colData(x)[[name]] <- value
  x
}

#' @export
dim.PlySummarizedExperiment <- function(x) {
  dim(x@se)
}

S7::S4_register(PlySummarizedExperiment)

#' @name PlySummarizedExperiment-methods
#' @title PlySummarizedExperiment Methods
#' @description
#' Methods from SummarizedExperiment package re-implemented for PlySummarizedExperiment.
#' @param x PlySummarizedExperiment object
#' @param withDimnames logical
#' @param ... additional arguments
#' @param i character or numeric index
#' @param value replacement value
#' @param use.names logical
#' @return Replacement functions return a PlySummarizedExperiment object. Other functions will return the same object as the method from SummarizedExperiment.
#' @examples
#' assays(se_simple)
#' rowData(se_simple)
#' colData(se_simple)
#'
NULL

#' @describeIn PlySummarizedExperiment-methods get the assays o the PlySummarizedExperiment object
#' @export
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

#' @describeIn PlySummarizedExperiment-methods set the assays of the PlySummarizedExperiment object
#' @export
setMethod(
  "assays<-", c("PlySummarizedExperiment", "list"),
  set_assays
)

#' @describeIn PlySummarizedExperiment-methods set the assays of the PlySummarizedExperiment object
#' @export
setMethod(
  "assays<-", c("PlySummarizedExperiment", "SimpleList"),
  set_assays
)


get_assay <- function(x, i, withDimnames = TRUE, ...) {
  assay(x@se, i = i, withDimnames = withDimnames, ...)
}

#' @describeIn PlySummarizedExperiment-methods get the first assay of the PlySummarizedExperiment object
#' @export
setMethod(
  "assay", c("PlySummarizedExperiment", "missing"),
  function(x, i, withDimnames = TRUE, ...) {
    assay(x@se, withDimnames = withDimnames, ...)
  }
)

#' @describeIn PlySummarizedExperiment-methods get assay from a PlySummarizedExperiment object
#' @export
setMethod("assay", c("PlySummarizedExperiment", "numeric"), get_assay)

#' @describeIn PlySummarizedExperiment-methods get assay from a PlySummarizedExperiment object
#' @export
setMethod("assay", c("PlySummarizedExperiment", "character"), get_assay)

set_assay <- function(x, i, withDimnames = TRUE, ..., value) {
  plyxp(x, `assay<-`, i = i, withDimnames = withDimnames, ..., value = value)
}

#' @describeIn PlySummarizedExperiment-methods set assay in a PlySummarizedExperiment object
#' @export
setMethod(
  "assay<-", c("PlySummarizedExperiment", "missing"),
  function(x, i, withDimnames = TRUE, ..., value = value) {
    plyxp(x, `assay<-`, withDimnames = withDimnames, ..., value = value)
  }
)

#' @describeIn PlySummarizedExperiment-methods set assay in a PlySummarizedExperiment object
#' @export
setMethod("assay<-", c("PlySummarizedExperiment", "numeric"), set_assay)

#' @describeIn PlySummarizedExperiment-methods set assay in a PlySummarizedExperiment object
#' @export
setMethod("assay<-", c("PlySummarizedExperiment", "character"), set_assay)

#' @describeIn PlySummarizedExperiment-methods get rowData in a PlySummarizedExperiment object
#' @export
setMethod(
  "rowData", "PlySummarizedExperiment",
  function(
      x,
      use.names = TRUE, ...) {
    rowData(x@se, use.names = use.names, ...)
  }
)

#' @describeIn PlySummarizedExperiment-methods set rowData in a PlySummarizedExperiment object
#' @export
setMethod(
  "rowData<-", "PlySummarizedExperiment",
  function(
      x, ..., value) {
    plyxp(x, `rowData<-`, ..., value = value)
  }
)

#' @describeIn PlySummarizedExperiment-methods get colData in a PlySummarizedExperiment object
#' @export
setMethod(
  "colData", "PlySummarizedExperiment",
  function(x, ...) {
    colData(x@se, ...)
  }
)

#' @describeIn PlySummarizedExperiment-methods set colData in a PlySummarizedExperiment object
#' @export
setMethod(
  "colData<-",
  c("PlySummarizedExperiment", "DataFrame"),
  function(x, ..., value) {
    plyxp(x, `colData<-`, ..., value = value)
  }
)

#' @rdname PlySummarizedExperiment-methods
#' @export
setMethod(
  "colData<-",
  c("PlySummarizedExperiment", "NULL"),
  function(x, ..., value) {
    plyxp(x, `colData<-`, ..., value = value)
  }
)
