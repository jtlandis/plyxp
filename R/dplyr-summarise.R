#' @importFrom dplyr summarize summarise
NULL

#' @name summarize
#' @title Summarize PlySummarizedExperiment
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @param ... expressions to summarize the object
#' @param .retain This argument controls how `rowData()` or `colData()` is retained
#' after summarizing. When "auto" (the default), `.retain` behavior depends on
#' the groupings of `.data`. When exactly one dimension is grouped, "auto"
#' behaves like "ungrouped-dim", and "none" otherwise. When "ungrouped-dim",
#' the ungrouped dimension's data are retained in the resulting
#' `SummarizedExperiment` object and scalar outputs are recycled to the length
#' of the ungrouped dimension. When "none", all outputs are expected to be
#' scalar and only computed values are retained in `rowData()` and `colData()`
#' @aliases summarise
#' @return an object inheriting PlySummarizedExperiment class
#' @examples
#'
#' # outputs in assay context may be either
#' # length 1, or the length of the ungrouped
#' # dimension while .retain = "auto"/"ungrouped-dim"
#' se_simple |>
#'   group_by(rows(direction)) |>
#'   summarise(
#'     col_sums = colSums(counts),
#'     sample = sample(1:20, 1L)
#'   )
#'
#' # .retain = "none" will drop ungrouped dimensions and
#' # outputs of assay context should be length 1.
#' se_simple |>
#'   group_by(rows(direction)) |>
#'   summarise(
#'     col_sums = list(colSums(counts)),
#'     .retain = "none"
#'   )
#'
#' # using an `across()` function will help
#' # nest ungrouped dimensions
#' se_simple |>
#'   group_by(rows(direction)) |>
#'   summarise(
#'     col_sums = list(colSums(counts)),
#'     cols(across(everything(), list)),
#'     .retain = "none"
#'   )
#'
#' @export
summarize.PlySummarizedExperiment <- function(.data, ...,
                                              .retain = c("auto", "ungrouped", "none")) {
  plyxp(.data, summarize_se_impl, ..., .retain = .retain)
}

summarize_se_impl <- function(.data, ...,
                              .retain = c("auto", "ungrouped", "none")) {
  .env <- caller_env()
  .groups <- metadata(.data)[["group_data"]]
  .retain <- match.arg(.retain, choices = c("auto", "ungrouped", "none"))
  .retain <- switch(.retain,
    auto = !is.null(.groups),
    ungrouped = TRUE,
    none = FALSE
  )
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "summarise")
  quos <- plyxp_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (!"assays" %in% ctxs) {
    rlang::abort(
      message = c(
        "No assay context expression detected",
        "!" = "at least one assay context expression required",
        "i" = "consider nesting data via `summarise(se, counts = list(counts))`"
      )
    )
  }

  nms <- names(quos)
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  assay_chops <- mask_pull_chops(mask$masks[["assays"]])
  group_vars_ <- group_vars_se_impl(.data)
  row_data <- col_data <- NULL
  .nrow <- .ncol <- 1L
  row_chops_sizes <- col_chops_sizes <- 1L
  row_names <- col_names <- NULL
  grouped_rows <- is_grouped_rows(.groups)
  grouped_cols <- is_grouped_cols(.groups)
  if (grouped_rows || "rows" %in% ctxs) {
    # get all chop data, groups and evaled
    row_chops <- mask_pull_chops(
      mask$masks[["rows"]],
      union(
        group_vars_$row_groups,
        mask$masks[["rows"]]$added
      )
    )
    # some settings
    if (.retain && !grouped_rows) {
      row_chops_sizes <- .nrow <- nrow(.data)
    } else {
      row_chops_sizes <- .nrow <- 1L
      if (grouped_rows) {
        .nrow <- nrow(.groups$row_groups)
        # slice grouped columns
        row_chops[group_vars_$row_groups] <- map(
          row_chops[group_vars_$row_groups],
          function(group_vec) {
            map(group_vec, .subset, 1L)
          }
        )
      }
    }
    # recycle each chop to required length
    row_chops <- assert_chops_size(row_chops, size = row_chops_sizes)
    row_data <- methods::new(
      "DFrame",
      listData = map(
        row_chops,
        vctrs::list_unchop
      ),
      nrows = .nrow
    )
  }
  if (grouped_cols || "cols" %in% ctxs) {
    # get all of the chops, including any groups
    col_chops <- mask_pull_chops(
      mask$masks[["cols"]],
      union(group_vars_$col_groups, mask$masks[["cols"]]$added)
    )
    # settings
    if (.retain && !grouped_cols) {
      col_chops_sizes <- .ncol <- ncol(.data)
    } else {
      col_chops_sizes <- .ncol <- 1L
      if (grouped_cols) {
        .ncol <- nrow(.groups$col_groups)
        # if grouped, grab only the first instance
        col_chops[group_vars_$col_groups] <- map(
          col_chops[group_vars_$col_groups],
          function(group_vec) {
            map(group_vec, .subset, 1L)
          }
        )
      }
    }
    # recycle each chop to required length
    col_chops <- assert_chops_size(col_chops, size = col_chops_sizes)
    col_data <- methods::new(
      "DFrame",
      listData = map(
        col_chops,
        vctrs::list_unchop
      ),
      nrows = .ncol
    )
  } else {
    col_data <- methods::new("DFrame",
      listData = set_names(list(), character()),
      nrows = .ncol
    )
  }
  # finally, if retained and not grouped, reassign back
  # to original row_data/col_data
  if (.retain) {
    if (!grouped_rows) {
      row_chops_sizes <- .nrow <- nrow(.data)
      row_data <- replace(rowData(.data), names(row_data), row_data)
    }
    if (!grouped_cols) {
      col_chops_sizes <- .ncol <- ncol(.data)
      col_data <- replace(colData(.data), names(col_data), col_data)
    }
  }

  new_metadata <- metadata(.data)
  if (group_type(.groups) != "none") {
    new_metadata$group_data <- plyxp_groups(
      row_data[group_vars_$row_groups],
      col_data[group_vars_$col_groups]
    )
  }

  if (".features" %in% names(row_data)) {
    row_names <- row_data$.features
    row_data$.features <- NULL
  }
  if (".samples" %in% names(col_data)) {
    col_names <- col_data$.samples
    col_data$.samples <- NULL
  }

  # we should have some type of value to view from
  # assays as it was enforced earlier.
  assay_data <- assert_chops_size(assay_chops,
    size = row_chops_sizes * col_chops_sizes
  ) |>
    map(
      vctrs::list_unchop
    ) |>
    map(
      matrix,
      nrow = .nrow,
      ncol = .ncol
    )

  out <- SummarizedExperiment(
    assays = assay_data,
    rowData = row_data,
    colData = col_data,
    metadata = new_metadata,
    checkDimnames = FALSE
  )
  if (!is.null(row_names)) {
    rownames(out) <- row_names
  }
  if (!is.null(col_names)) {
    colnames(out) <- col_names
  }
  out
}


#' @rdname summarize
#' @export
summarise.PlySummarizedExperiment <- summarize.PlySummarizedExperiment



assert_chops_size <- function(chops, size = 1L) {
  imap(
    chops,
    function(vec, name, size) {
      # get the underlying vec_recycle
      # method for this vec class
      fn <- method(vec_recycle, object = vec[[1]])
      map(vec, fn, size = size, x_arg = name)
    },
    size = size
  )
}

mask_pull_chops <- function(mask, names = NULL) {
  chop_env <- mask$environments@env_data_chop
  names <- names %||% mask$added
  names(names) <- names
  lapply(names, function(name) chop_env[[name]])
}

get_mask_chops <- function(mask) {
  lapply(mask$masks, mask_pull_chops)
}
