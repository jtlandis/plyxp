#' @name new_plyxp
#' @title SummarizedExperiment Shell Object
#' @description
#' A container object for the SummarizedExperiment class.
#' @param se SummarizedExperiment object
#' @aliases PlySummarizedExperiment
#' @return PlySummarizedExperiment object
#' @examples
#' se <- SummarizedExperiment(
#'   assays = list(counts = matrix(1:6, nrow = 3)),
#'   colData = S4Vectors::DataFrame(condition = c("A", "B"))
#' )
#' new_plyxp(se = se)
#' # or
#' PlySummarizedExperiment(se = se)
#' @export
new_plyxp <- function(se) {
  PlySummarizedExperiment(se = se)
}

#' @importClassesFrom SummarizedExperiment SummarizedExperiment

#' @rdname new_plyxp 
#' @description 
#' This S4 class is implemented to bring unique `dplyr` 
#' syntax to the `SummarizedExperiment` object without clashing with the
#' `tidySummarizedExperiment` package. As such, this is a simple wrapper that
#' contains one slot, which holds a `SummarizedExperiment` object.
#' @slot se contains the underlying `SummarizedExperiment` class.
#' @export
setClass(
  "PlySummarizedExperiment",
  slots = c(
    se = "SummarizedExperiment"
  )
)

#' @rdname new_plyxp
#' @export
PlySummarizedExperiment <- function(se) {
  obj <- new("PlySummarizedExperiment")
  obj@se <- se
  obj
}

setMethod(
  "show", "PlySummarizedExperiment",
  function(object) {
    show_tidy(se(object))
  }
)

#' @rdname PlySummarizedExperiment-methods
#' @export
setGeneric("se", function(x) standardGeneric("se"))

#' @describeIn PlySummarizedExperiment-methods get the se slot of the PlySummarizedExperiment object
#' @export
setMethod("se", "PlySummarizedExperiment", function(x) x@se)

#' @describeIn PlySummarizedExperiment-methods set the se slot of the PlySummarizedExperiment object
#' @export
setGeneric("se<-", function(x, value) standardGeneric("se<-"))

#' @describeIn PlySummarizedExperiment-methods set the se slot of the PlySummarizedExperiment object
#' @export
setMethod("se<-", "PlySummarizedExperiment", function(x, value) {
  x@se <- value
  x
})

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
  out <- .f(se(.data), ...)
  if (!methods::is(out, "SummarizedExperiment")) {
    cli::cli_abort("{.arg .f} must return a {.cls SummarizedExperiment} object")
  }
  se(.data) <- out
  .data
}

#' @export
print.PlySummarizedExperiment <- function(x, ...) {
  show_tidy(se(x), ...)
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
  se(x) <- switch(type,
    i = se(x)[i, ],
    j = se(x)[, j],
    ij = se(x)[i, j],
    se(x)
  )
  x
}

#' @export
`$.PlySummarizedExperiment` <- function(x, name) {
  colData(se(x))[[name]]
}

#' @export
`$<-.PlySummarizedExperiment` <- function(x, name, value) {
  colData(x)[[name]] <- value
  x
}

#' @export
dim.PlySummarizedExperiment <- function(x) {
  dim(se(x))
}

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
    assays(se(x), withDimnames = withDimnames, ...)
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
  assay(se(x), i = i, withDimnames = withDimnames, ...)
}

#' @describeIn PlySummarizedExperiment-methods get the first assay of the PlySummarizedExperiment object
#' @export
setMethod(
  "assay", c("PlySummarizedExperiment", "missing"),
  function(x, i, withDimnames = TRUE, ...) {
    assay(se(x), withDimnames = withDimnames, ...)
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
  function(x,
           use.names = TRUE, ...) {
    rowData(se(x), use.names = use.names, ...)
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
    colData(se(x), ...)
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
