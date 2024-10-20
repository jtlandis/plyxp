

#' @title select assays, rowData, and colData names
#' @description
#' Select one or more values from each context. By default omitting an expression
#' for a context is the same as selecting NOTHING from that context.
#'
#' The <[`tidy-select`][dplyr::dplyr_tidy_select]> implementation within
#' `plyxp` is almost similar to `dplyr` except when used within the
#' `across()` function. When used from `accross()`, the data provided to
#' [eval_select][tidyselect::eval_select] is a zero length slice of the data.
#' This was an intentional choice to prevent the evaluation of potentionally
#' expensive chopping operations for S4Vectors. This means that predicate
#' function from [`where()`][tidyselect::where] will NOT be able to query the
#' original data.
#'
#'
#' @param .data a `SummarizedExperiment` object
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> one or more selection
#' expressions. Supports wrapping expressions within the
#' <[`plyxp-contexts`][plyxp::plyxp-context]>.
#' @return an object inheriting SummarizedExperiment class
#' @examples
#'
#'
#' # only keep assays, other contexts are dropped
#' select(se_simple, everything())
#'
#' # only keep rowData, other contexts are dropped
#' select(se_simple, rows(everything()))
#'
#' select(se_simple, rows(where(is.numeric)))
#'
#' # Note on `where()` clause, all data is available within select
#' select(se_simple, rows(where(~any(grepl("-", .x)))))
#'
#' # within an `across()`, only a zero-length slice avialble, so the
#' # `where()` predicate cannot access the data
#' mutate(se_simple,
#'        rows(
#'         across(where(~any(grepl("-", .x))),
#'                ~sprintf("%s foo", .x))))
#' # here is an acceptable usage of the `where()` predicate
#' mutate(se_simple,
#'        rows(
#'         across(where(is.character),
#'                ~sprintf("%s foo", .x))))
#'
#' @export
select.PlySummarizedExperiment <- function(.data, ...) {
  plyxp(.data, select_se_impl, ...)
}

select_se_impl <- function(.data, ...) {

  .env <- caller_env()
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::dplyr_verb", "select")
  quos <- plyxp_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  nms  <- names(quos)


  selected <- plyxp_eval_select(
    quos = quos, ctxs = ctxs, data = .data
  )


  assays(.data) <- assays(.data)[selected[["assays"]]]
  rowData(.data) <- rowData(.data)[selected[["rows"]]]
  colData(.data) <- colData(.data)[selected[["cols"]]]

  .data

}

# underlying selection function. This may need to be an S3 generic
# for when `plyxp` extends to other classes
plyxp_eval_select <- function(quos, ctxs, data) {

  out <- vector("list", length(quos))
  .data <- list(
    assays = as.list(assays(data)),
    rows = as.list(rowData(data)),
    cols = as.list(colData(data))
  )

  for (i in seq_along(quos)) {
    quo <- quos[[i]]
    ctx <- ctxs[i]
    out[[i]] <- tidyselect::eval_select(quo, .data[[ctx]])
  }

  ctxs <- factor(ctxs, levels = c("assays","rows","cols"))
  split(out, ctxs) |>
    map(function(.x) {
      unlist(.x) |>
        unique()
    })

}
