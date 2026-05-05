# Meant to be used to propagate NULL when a downstream
# function could take NULLs
`%|!|%` <- function(x, y) if (!is_empty(x)) y else NULL

#' @name filter
#' @title filter PlySummarizedExperiment
#' @description
#'
#' The `filter()` function is used to subset an object, returning the observations
#' that satisfy your conditions. An observation must return TRUE for all conditions
#' within a context to be retained. Note, to guarantee a valid
#' `PlySummarizedExperiment` is returned, filtering in the `assays` evaluation
#' context is disabled.
#'
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @param ... conditions to filter on. These must be wrapped in `cols()` and or
#' `rows()`
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE
#' (the default), the grouping structure is recalculated based on the resulting data,
#' i.e. the number of groups may change.
#' @return an object inheriting `PlySummarizedExperiment` class
#' @examples
#' # example code
#' filter(
#'   se_simple,
#'   rows(length > 30),
#'   cols(condition == "drug")
#' )
#'
#' filter(
#'   se_simple,
#'   rows(rowSums(.assays_asis$counts) > 40),
#'   cols(colSums(.assays_asis$counts) < 50)
#' )
#'
#' # assay context is disabled
#' filter(
#'   se_simple,
#'   counts > 12
#' ) |> try()
#'
#' # convert to `data.frame` first
#' as.data.frame(se_simple) |>
#'   filter(counts > 12)
#'
#' @export
filter.PlySummarizedExperiment <- function(.data, ..., .preserve = FALSE) {
  plyxp(.data, filter_se_impl, ..., .preserve = .preserve)
}

filter_se_impl <- function(.data, ..., .preserve = FALSE) {
  .env <- caller_env()
  .groups <- group_data_se_impl(.data)
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "filter")
  quos <- plyxp_quos(..., .ctx = c("assays", "rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = "filter", review = err)
  }
  nms <- names(quos)
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  filter_ <- ""
  if (!is_empty(results$rows)) {
    row_logic <- vctrs::vec_cast_common(
      splice(results$rows),
      .to = logical()
    ) |>
      splice() |>
      vec_recycle_common() |>
      reduce(`&`)
    filter_ <- "row"
  }
  if (!is_empty(results$cols)) {
    col_logic <- vctrs::vec_cast_common(
      splice(results$cols),
      .to = logical()
    ) |>
      splice() |>
      vec_recycle_common() |>
      reduce(`&`)
    filter_ <- paste0(filter_, "col")
  }
  switch(filter_,
    rowcol = plyxp_slice_se(.data, row_logic, col_logic, .preserve = .preserve),
    row = plyxp_slice_se(.data, row_logic, .preserve = .preserve),
    col = plyxp_slice_se(.data, , col_logic, .preserve = .preserve),
    .data
  )
}
