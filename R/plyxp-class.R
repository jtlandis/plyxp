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
  "show",
  "PlySummarizedExperiment",
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
#' @param .f within `plyxp()`: a function that returns a SummarizedExperiment object.
#' within `plyxp_on()`: `.f` should return a value compatible with `.on(se)<-`
#' @param ... additional arguments passed to `.f`
#' @param .caller environment in which plyxp should signal an error if one occurs.
#' @return a PlySummarizedExperiment object
#' @examples
#' plyxp(se_simple, function(x) x)
#' @export
plyxp <- function(.data, .f, ..., .caller = caller_env()) {
  # browser()
  plyxp_function <- substitute(.f)
  .f <- rlang::as_function(.f)
  out <- try_fetch(
    .f(se(.data), ...),
    plyxp_on_failure = function(cnd) {
      abort(class = "plyxp_failure", parent = cnd, call = NULL)
    },
    error = function(cnd) {
      message <- NULL
      if (is_call(cnd$call, ".f")) {
        if (is_call(plyxp_function)) {
          # .f is likely an anonymous function
          cnd$call <- NULL
          message <- c(
            "error in plyxp()",
            "i" = "check the function passed to `.f` argument"
          )
        } else {
          cnd$call[[1]] <- plyxp_function
        }
      }
      abort(
        message = message,
        class = "plyxp_failure",
        parent = cnd,
        call = .caller
      )
    }
  )
  if (!methods::is(out, "SummarizedExperiment")) {
    cli::cli_abort("{.arg .f} must return a {.cls SummarizedExperiment} object")
  }
  se(.data) <- out
  .data
}

#' @describeIn plyxp pass a function to the result of an accessor of the
#' `SummarizedExperiment` Class
#' This function is a wrapper for the expression:
#' \preformatted{
#'  plyxp::plyxp(.data, function(se, ...) {
#'    .f <- rlang::as_function(.f)
#'    obj <- .on(se)
#'    obj <- .f(se, ...)
#'    .on(se) <- obj
#'    se
#'  }, ...)
#' }
#' where `.on` is the symbol for the accessor function into a
#' `SummarizedExperiment` Class. Note: the setter variant must exist in the
#' environment that `plyxp_on()` is called. All other arguments are diffused
#' as quosures and will be evaluated in the environment they were quoted.
#' @param .on a symbol matching an accessor and setter function for the
#' `SummarizedExperiment` Class.
#' @examples
#' plyxp_on(se_simple,
#'         .f = lapply, # function to call on `.on` args,
#'         .on = rowData, # data `.f` will be used on
#'          paste, "foo" # arguments for `.f`)
#' @export
plyxp_on <- function(.data, .f, ..., .on, .caller = caller_env()) {
  .on_sub <- substitute(.on)
  .f_sub <- substitute(.f)
  .on_name <- rlang::as_string(.on_sub)
  .on <- find_func(.on_name)
  `.on<-` <- find_func(sprintf("%s<-", .on_name))
  this_call <- caller_call(0)
  inner_function <- function(se, ...) {
    .f <- rlang::as_function(.f)
    try_fetch(
      {
        obj <- .on(se)
        obj <- .f(obj, ...)
        .on(se) <- obj
      },
      error = function(cnd) {
        message <- NULL
        case_call(
          cnd$call,
          .on = {
            cnd$call[[1]] <- sym(.on_name)
            message <- sprintf("error in `%s(se(.data))", .on_name)
          },
          .f = {
            cnd$call[[1]] <- .f_sub
            message <- sprintf(
              "error in `%s(%s(se(.data)), ...)",
              as_label(.f_sub),
              .on_name
            )
          },
          `.on<-` = {
            cnd$call[[1]] <- sym(sprintf("%s<-", .on_name))
            message <- sprintf("error in `%s(se(.data)) <- value`", .on_name)
          }
        )
        abort(
          message = message,
          class = "plyxp_on_failure",
          parent = cnd,
          plyxp_on_call = .caller,
          call = this_call
        )
      }
    )
    se
  }
  plyxp(.data, .f = inner_function, ..., .caller = .caller)
}

case_call <- function(x, ...) {
  env <- parent.frame(1)
  if (is.call(x)) {
    call_arg <- as_label(x[[1]])
    dots <- enexprs(...)
    expr <- expr(switch(!!call_arg, !!!dots))
    eval(expr, envir = env)
  }
}

find_func <- function(func, .caller = caller_env()) {
  try_fetch(match.fun(func), error = function(cnd) {
    abort(sprintf("could not match %s()", func), parent = cnd, call = .caller)
  })
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
  se(x) <- switch(type, i = se(x)[i, ], j = se(x)[, j], ij = se(x)[i, j], se(x))
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
  "assays",
  "PlySummarizedExperiment",
  function(x, withDimnames = TRUE, ...) {
    assays(se(x), withDimnames = withDimnames, ...)
  }
)

set_assays <- function(
  x,
  withDimnames = TRUE,
  ...,
  value
) {
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
  "assays<-",
  c("PlySummarizedExperiment", "list"),
  set_assays
)

#' @describeIn PlySummarizedExperiment-methods set the assays of the PlySummarizedExperiment object
#' @export
setMethod(
  "assays<-",
  c("PlySummarizedExperiment", "SimpleList"),
  set_assays
)

get_assay <- function(x, i, withDimnames = TRUE, ...) {
  assay(se(x), i = i, withDimnames = withDimnames, ...)
}

#' @describeIn PlySummarizedExperiment-methods get the first assay of the PlySummarizedExperiment object
#' @export
setMethod(
  "assay",
  c("PlySummarizedExperiment", "missing"),
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
  "assay<-",
  c("PlySummarizedExperiment", "missing"),
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
  "rowData",
  "PlySummarizedExperiment",
  function(x, use.names = TRUE, ...) {
    rowData(se(x), use.names = use.names, ...)
  }
)

#' @describeIn PlySummarizedExperiment-methods set rowData in a PlySummarizedExperiment object
#' @export
setMethod(
  "rowData<-",
  "PlySummarizedExperiment",
  function(
    x,
    ...,
    value
  ) {
    plyxp(x, `rowData<-`, ..., value = value)
  }
)

#' @describeIn PlySummarizedExperiment-methods get colData in a PlySummarizedExperiment object
#' @export
setMethod(
  "colData",
  "PlySummarizedExperiment",
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
