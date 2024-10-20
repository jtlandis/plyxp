# Meant to be used to propagate NULL when a downstream
# function could take NULLs
`%|!|%` <- function(x, y) if (!is_empty(x)) y else NULL

#' @title Filter SummarizedExperiment
#' @description
#'
#' The `filter()` function is sused to subset an object, returing the observations
#' that satisfy your conditions. An observation must return TRUE for all conditions
#' within a context to be retained. Note, to guarantee a valid
#' `SummarizedExperiment` is returned, filtering in the `assays` evaluation
#' context is disabled.
#'
#' @param .data A SummarizedExperiment object
#' @param ... conditions to filter on. These must be wrapped in `cols()` and or
#' `rows()`
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE
#' (the default), the grouping structure is recalculated based on the resulting data,
#' i.e. the number of groups may change.
#' @return an object inheriting SummarizedExperiment class
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
  .groups <- metadata(.data)[["group_data"]]
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "filter")
  quos <- plyxp_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    abort(
      message = c(
        "Cannot filter in `assays` context",
        "x" = sprintf(
          "review expression indices %s in dots",
          paste0(which(err), collapse = ", ")
        ),
        "i" = "consider wrapping expressions in rows(...) or cols(...)"
      )
    )
  }
  nms <- names(quos)
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  filter_ <- ""
  if (!is_empty(results$rows)) {
    row_logic <- vec_recycle_common(splice(results$rows)) |>
      reduce(`&`)
    filter_ <- "row"
  }
  if (!is_empty(results$cols)) {
    col_logic <- vec_recycle_common(splice(results$cols)) |>
      reduce(`&`)
    filter_ <- paste0(filter_, "col")
  }
  .data <- switch(filter_,
    rowcol = .data[row_logic, col_logic],
    row = .data[row_logic, ],
    col = .data[, col_logic],
    .data
  )
  current_groups <- metadata(.data)[["group_data"]]
  if (is.null(current_groups)) {
    return(.data)
  }
  row_select <- grep("^.indices", names(current_groups[["row_groups"]]),
    value = TRUE, invert = TRUE
  )
  row_groups <- row_select %|!|% as_tibble(rowData(.data), rownames = ".features")[row_select]
  col_select <- grep("^.indices", names(current_groups[["col_groups"]]),
    value = TRUE, invert = TRUE
  )
  col_groups <- col_select %|!|% as_tibble(colData(.data), rownames = ".samples")[col_select]
  new_groups <- plyxp_groups(
    row_groups = row_groups,
    col_groups = col_groups
  )
  if (.preserve) {
    if (!is_empty(current_groups$row_groups)) {
      current_groups$row_groups$.indices[] <- list(integer())
      new_groups$row_groups <- rows_update(
        current_groups$row_groups,
        new_groups$row_groups,
        by = row_select
      )
    }
    if (!is_empty(current_groups$col_groups)) {
      current_groups$col_groups$.indices[] <- list(integer())
      new_groups$col_groups <- rows_update(
        current_groups$col_groups,
        new_groups$col_groups,
        by = col_select
      )
    }
  }
  metadata(.data)[["group_data"]] <- new_groups
  .data
}
