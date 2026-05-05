#' @name group_data
#' @title get grouping data
#' @description
#' retrieve grouping information from a `SummarizedExperiment` object. This
#' is stored within the `metadata()` of the object.
#' @aliases ungroup
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @return list of groupings for an SummarizedExperiment
#' @examples
#' group_by(se_simple, rows(direction), cols(condition)) |> group_data()
#' @export
group_data.PlySummarizedExperiment <- function(.data) {
  group_data_se_impl(se(.data))
}

group_data_se_impl <- function(.data) {
  out <- list(
    row_groups = group_data_DF_impl(rowData(.data)),
    col_groups = group_data_DF_impl(colData(.data))
  )
  if (all(vapply(out, is.null, FUN.VALUE = FALSE))) {
    return(NULL)
  }
  structure(out, class = "plyxp_groups")
}

# when setting grouping data, if field is absent or NULL
# then it will have the effect of removing said group.
`group_data_se_impl<-` <- function(.data, value) {
  row_groups <- value[["row_groups"]]
  group_data_DF_impl(rowData(.data)) <- row_groups
  col_groups <- value[["col_groups"]]
  group_data_DF_impl(colData(.data)) <- col_groups
  .data
}

group_data_DF_impl <- function(.data) {
  metadata(.data)[["group_data"]]
}

`group_data_DF_impl<-` <- function(.data, value) {
  if (is.null(value)) {
    metadata(.data)["group_data"] <- value
  } else {
    metadata(.data)[["group_data"]] <- value
  }

  .data
}


#' @name group_by
#' @title apply groups to PlySummarizedExperiment
#' @description
#' create grouping variables about the rowData and colData of a
#' `PlySummarizedExperiment` object. Unlike the `data.frame` method
#' the resulting output class is left unchanged. Thus `dplyr` generics for
#' `PlySummarizedExperiment` must check grouping information manually.
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#'
#' ## S4 Compatibility
#'
#' At the moment, grouping on S4 Vectors is not yet supported. This is due to
#' `plyxp` using `[vec_group_loc][vctrs::vec_group_loc]` to form grouping
#' information. `plyxp` will eventually develop a method to handle S4 Vectors.
#'
#' @param ... expressions to group on. Grouping may only be done on
#' rowData and/or colData by `rows()` and `cols()` respectively.
#' @param .add When `FALSE`, the default, `group_by()` will override
#' existing groups.
#' @return `PlySummarizedExperiment` object
#' @examples
#'
#' group_by(se_simple, rows(direction), cols(condition))
#'
#' @export
group_by.PlySummarizedExperiment <- function(.data, ..., .add = FALSE) {
  plyxp(.data, group_by_se_impl, ..., .add = .add)
}

group_by_se_impl <- function(.data, ..., .add = FALSE) {
  #
  .env <- caller_env()
  # to maintain consistency with dplyr
  # force any computations to occur on ungrouped data
  .groups <- group_data_se_impl(.data)
  group_data_se_impl(.data) <- NULL
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "group_by")
  quos <- plyxp_quos(..., .ctx = c("assays", "rows", "cols"))
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = "group_by", review = err)
  }
  nms <- names(quos)
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()
  # nms <- names(results$assays)
  # for (i in seq_along(results$assays)) {
  #   assays(.data, withDimnames = FALSE)[[nms[i]]] <- results$assays[[i]]
  # }

  if (.add && !is_empty(curr_groups <- group_data_se_impl(.data))) {
    if (!is_empty(curr_groups$row_groups)) {
      curr <- select(curr_groups$row_groups, -starts_with(".indices")) |>
        names()
      curr <- rowData(.data)[curr]
      curr[names(results$rows)] <- results$rows
      results$rows <- curr
    }
    if (!is_empty(curr_groups$col_groups)) {
      curr <- select(curr_groups$col_groups, -starts_with(".indices")) |>
        names()
      curr <- colData(.data)[curr]
      curr[names(results$cols)] <- results$cols
      results$cols <- curr
    }
  }

  rnms <- names(results$rows)
  if (".features" %in% rnms) {
    rownames(.data) <- results$rows$.features
    results$rows$.features <- NULL
    rnms <- names(results$rows)
  }
  if (length(rnms)) {
    row_data <- rowData(.data)
    row_data[rnms] <- results$rows
    # push nms to the front
    row_data <- row_data[c(rnms, setdiff(names(row_data), rnms))]
    rowData(.data) <- row_data
  }


  cnms <- names(results$cols)
  if (".samples" %in% cnms) {
    colnames(.data) <- results$cols$.samples
    results$cols$.samples <- NULL
    cnms <- names(results$cols)
  }
  if (length(cnms)) {
    col_data <- colData(.data)
    col_data[cnms] <- results$cols
    # push nms to the front
    col_data <- col_data[c(cnms, setdiff(names(col_data), cnms))]
    colData(.data) <- col_data
  }
  groups <- plyxp_groups(
    row_groups = rowData(.data)[rnms],
    col_groups = colData(.data)[cnms]
  )
  group_data_se_impl(.data) <- groups
  .data
}

#' @describeIn group_by Ungroup a PlySummarizedExperiment object
#'
#' @param x An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @param ... [contextual expressions][plyxp::plyxp-context] specifying
#' which columns to ungroup. Omitting `...` ungroups the entire object.
#' @export
ungroup.PlySummarizedExperiment <- function(x, ...) {
  plyxp(x, ungroup_se_impl, ...)
}

ungroup_se_impl <- function(x, ...) {
  quos <- plyxp_quos(
    ...,
    .named = FALSE,
    .ctx = c("assays", "rows", "cols")
  )
  curr_groups <- group_data_se_impl(x)
  if (is_empty(curr_groups)) {
    return(x)
  }
  n_quo <- length(quos)
  if (n_quo == 0L) {
    group_data_se_impl(x) <- NULL
    return(x)
  }
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = "ungroup", review = err)
  }
  by_ctx <- split(quos, ctxs)
  update_cols <- update_rows <- NULL
  update_ <- ""
  if (!is_empty(by_ctx$rows) || !is.null(curr_groups$row_groups)) {
    if (is.null(curr_groups$row_groups)) {
      abort("no row groups to ungroup")
    }
    old_groups <- select(curr_groups$row_groups, -starts_with(".indices")) |>
      names()
    select_expr <- call2("c", splice(by_ctx$rows))
    to_remove <- eval_select(
      select_expr,
      data = as.list(rowData(x)),
      allow_rename = FALSE
    )
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_rows <- call2("rows", splice(syms(new_groups)))
    update_ <- "row"
  }
  if (!is_empty(by_ctx$cols) || !is.null(curr_groups$col_groups)) {
    if (is.null(curr_groups$col_groups)) {
      abort("no col groups to ungroup")
    }
    old_groups <- select(curr_groups$col_groups, -starts_with(".indices")) |>
      names()
    select_expr <- call2("c", splice(by_ctx$cols))
    to_remove <- eval_select(
      select_expr,
      data = as.list(colData(x)),
      allow_rename = FALSE
    )
    to_remove <- names(to_remove)
    new_groups <- setdiff(old_groups, to_remove)
    update_cols <- call2("cols", splice(syms(new_groups)))
    update_ <- paste0(update_, "col")
  }
  switch(update_,
    rowcol = group_by_se_impl(x, !!update_rows, !!update_cols),
    row = group_by_se_impl(x, !!update_rows),
    col = group_by_se_impl(x, !!update_cols)
  )
}

#' @export
groups.PlySummarizedExperiment <- function(x) {
  groups_se_impl(se(x))
}

groups_se_impl <- function(x) {
  map(
    group_vars_se_impl(x),
    syms
  )
}

plyxp_curr_groups <- function(x) {
  vars <- groups_se_impl(x)
  row_v <- if (is_empty(vars$row_groups)) {
    NULL
  } else {
    call2("rows", splice(vars$row_groups))
  }
  col_v <- if (is_empty(vars$col_groups)) {
    NULL
  } else {
    call2("cols", splice(vars$col_groups))
  }
  c(row_v, col_v)
}

#' @name group_split
#' @title Split a PlySummarizedExperiment based on groups
#'
#' @description
#' Splits a grouped PlySummarizedExperiment based on groups. Note the elements
#' of the return value are ungrouped PlySummarizedExperiment objects.
#'
#' @param .tbl a PlySummarizedExperiment object
#' @param ... ignored if the `.tbl` is grouped, otherwise it is passed to
#' [plyxp::group_by].
#' @param .keep logical indicating of grouping variables should be kept
#' @return A list of PlySummarizedExperiment objects
#' @examples
#'
#' gse <- group_by(se_simple, rows(direction), cols(condition))
#' gse |> group_split()
#' gse |> group_split(.keep = FALSE)
#'
#' @export
group_split.PlySummarizedExperiment <- function(.tbl, ..., .keep = TRUE) {
  groups <- group_data_se_impl(.tbl)

  quos <- rlang::enquos(...)
  if (!is_empty(quos)) {
    if (!is.null(groups)) {
      rlang::warn("... is ignored when `.tbl` is grouped")
    } else {
      .tbl <- group_by(.tbl, !!!quos)
      groups <- group_data_se_impl(.tbl)
    }
  }
  # grab groups
  group_vars <- group_vars_se_impl(.tbl)

  # always ungroups resulting data
  group_data_se_impl(.tbl) <- NULL
  grouped_rows <- is_grouped_rows(group_vars)
  grouped_cols <- is_grouped_cols(group_vars)
  is_grouped <- grouped_rows || grouped_cols


  if (!.keep && is_grouped) {
    if (grouped_rows) {
      rowData(.tbl) <- rowData(.tbl)[,
        setdiff(
          names(rowData(.tbl)),
          group_vars$row_groups
        ),
        drop = FALSE
      ]
    }
    if (grouped_cols) {
      colData(.tbl) <- colData(.tbl)[,
        setdiff(
          names(colData(.tbl)),
          group_vars$col_groups
        ),
        drop = FALSE
      ]
    }
  }
  type <- paste0(
    c("rows", "cols")[c(grouped_rows, grouped_cols)],
    collapse = ""
  )
  switch(type,
    rows = lapply(
      groups$row_groups$.indices,
      \(i, .data) new_plyxp(.data[i, ]),
      .data = se(.tbl)
    ),
    cols = lapply(
      groups$col_groups$.indices,
      \(j, .data) new_plyxp(.data[, j]),
      .data = se(.tbl)
    ),
    rowscols = map2(
      vctrs::vec_rep(groups$row_groups$.indices, nrow(groups$col_groups)),
      vctrs::vec_rep_each(groups$col_groups$.indices, nrow(groups$row_groups)),
      \(i, j, .data) new_plyxp(.data[i, j]),
      .data = se(.tbl)
    ),
    list(.tbl)
  )
}
