
#' @title extract data from object
#' @description
#' similar to `dplyr::pull.data.frame` except allows to extract objects
#' from different contexts.
#' @param .data a SummarizedExperiment object
#' @param var A variable as specified by [dplyr::pull][dplyr::pull]
#' @param name ignored argument. Due to the range of data types a 
#' `SummarizedExperiment` this argument is not supported
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
pull.SummarizedExperiment <- function(.data, var = -1, name = NULL, ...) {
  # browser()
  rlang::check_dots_empty()
  .env <- caller_env()
  quos <- plyxp_quos({{ var }}, .ctx_default = "assays",
                        .ctx_opt = c("rows", "cols"))
  if (length(quos)>1) abort("`var` can only pull one object")
  if (!is.null(name)) warn("arg `name` is not used in pull.SummarizedExperiment()")
  var <- quos[[1]]
  ctxs <- attr(var, which = "plyxp:::ctx")
  data_ctx <- switch(ctxs,
                     "assays" = assays(.data),
                     "rows" = rowData(.data),
                     "cols" = colData(.data))
  var <- tidyselect::vars_pull(names(data_ctx), !!var)
  return(data_ctx[[var]])
}