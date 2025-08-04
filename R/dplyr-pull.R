#' @name pull
#' @title extract data from object
#' @description
#' similar to `dplyr::pull.data.frame` except allows to extract objects
#' from different contexts.
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @param var A variable as specified by [dplyr::pull][dplyr::pull]
#' @param name ignored argument. Due to the range of data types a
#' `PlySummarizedExperiment` this argument is not supported
#' @param ... unused argument
#' @return an element from either the assays, rowData, or colData of a
#' `SummarizedExperiment` object
#' @examples
#'
#' # last element of default context (assays)
#' pull(se_simple, var = -1)
#' # first element of rows context
#' pull(se_simple, var = rows(1))
#' # element from col context by literal variable name
#' pull(se_simple, var = cols(condition))
#'
#' # use `pull()` to return contextual info
#' mutate(se_simple, rows(counts = .assays$counts)) |>
#'   # get last stored element
#'   pull(rows(-1))
#' @export
pull.PlySummarizedExperiment <- function(.data, var = -1, name = NULL, ...) {
  pull_se_impl(se(.data), var = {{ var }}, name = name, ...)
}

pull_se_impl <- function(.data, var = -1, name = NULL, ...) {
  # browser()
  rlang::check_dots_empty()
  .env <- caller_env()
  quos <- plyxp_quos({{ var }},
    .ctx_default = "assays",
    .ctx_opt = c("rows", "cols")
  )
  if (length(quos) > 1) abort("`var` can only pull one object")
  if (!is.null(name)) warn("arg `name` is not used in pull.SummarizedExperiment()")
  var <- quos[[1]]
  ctxs <- attr(var, which = "plyxp:::ctx")
  data_ctx <- switch(ctxs,
    "assays" = assays(.data),
    "rows" = rowData(.data),
    "cols" = colData(.data)
  )
  requesting_anno_names <- FALSE
  var <- try_fetch(
    tidyselect::vars_select(names(data_ctx), !!var),
    error = function(cnd) {
      var <- rlang::as_label(var)
      if ((var == ".features" && ctxs == "rows") ||
        (var == ".samples" && ctxs == "cols")) {
        requesting_anno_names <<- TRUE
        return(var)
      }
      # rethrow error
      abort(sprintf("failed to pull '%s' from '%s' context", var, ctxs),
        parent = cnd, call = .env
      )
    }
  )

  if (requesting_anno_names) {
    rownames(data_ctx) %||% seq_len(nrows(data_ctx))
  } else {
    data_ctx[[var]]
  }
}
