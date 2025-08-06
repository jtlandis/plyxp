#' @name arrange
#' @title arrange rows or columns of PlySummarizedExperiment
#' @description
#' `arrange()` orders either the rows or columns of a `PlySummarizedExperiment`
#' object. Note, to guarentee a valid `PlySummarizedExperiment` is returned,
#' arranging in the `assays` evaluation context is disabled.
#'
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping. The
#' `PlySummarizedExperiment` method also provides the same functionality via the
#' `.by_group` argument.
#'
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @inheritParams dplyr::arrange
#' @return an object inheriting `PlySummarizedExperiment` class
#' @examples
#'
#' # arrange within rows/cols contexts separately
#' arrange(
#'   se_simple,
#'   rows(direction),
#'   cols(dplyr::desc(condition))
#' )
#'
#' # access assay data to compute arrangement
#' arrange(
#'   se_simple,
#'   rows(rowSums(.assays_asis$counts)),
#'   cols(colSums(.assays_asis$counts))
#' )
#'
#' # assay context is disabled
#' arrange(se_simple, counts) |> try()
#'
#' # convert to `data.frame` first
#' as.data.frame(se_simple) |>
#'   arrange(counts)
#'
#' @export
arrange.PlySummarizedExperiment <- function(.data, ..., .by_group = FALSE) {
  plyxp(.data, arrange_se_impl, ..., .by_group = .by_group)
}

arrange_se_impl <- function(.data, ..., .by_group = FALSE) {
  .env <- caller_env()
  quos <- plyxp_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))
  if (.by_group) {
    quos <- c(
      plyxp_quos(
        !!!plyxp_curr_groups(.data),
        .ctx_default = "assays",
        .ctx_opt = c("rows", "cols")
      ),
      quos
    )
  }
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = "arrange", review = err)
  }
  nms <- names(quos)
  # to make this function consistent
  groups <- group_data_se_impl(.data)
  metadata(.data)[["group_data"]] <- NULL
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "arrange")
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  type <- ""
  ro <- co <- NULL
  if (!is_empty(results$rows)) {
    type <- "row"
    ro <- exec("order", splice(results$rows), method = "radix")
  }

  if (!is_empty(results$cols)) {
    type <- paste0(type, "col")
    co <- exec("order", splice(results$cols), method = "radix")
  }

  out <- switch(type,
    rowcol = .data[ro, co],
    row = .data[ro, ],
    col = .data[, co],
    .data
  )

  if (!is.null(groups)) {
    if (!is_empty(groups$row_groups) && !is.null(ro)) {
      group_inds <- group_ind(groups$row_groups$.indices, nrow(.data))
      new_id <- vctrs::vec_slice(group_inds, ro)
      new_grps <- vec_group_loc(new_id)
      inds <- vector("list", nrow(groups$row_groups))
      inds[new_grps$key] <- new_grps$loc
      groups$row_groups$.indices <- inds
    }

    if (!is_empty(groups$col_groups) && !is.null(co)) {
      group_inds <- group_ind(groups$col_groups$.indices, ncol(.data))
      new_id <- vctrs::vec_slice(group_inds, co)
      new_grps <- vec_group_loc(new_id)
      inds <- vector("list", nrow(groups$col_groups))
      inds[new_grps$key] <- new_grps$loc
      groups$col_groups$.indices <- inds
    }

    metadata(out)[["group_data"]] <- groups
  }

  out
}
