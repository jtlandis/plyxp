#' @name slice
#' @title Slice a PlySummarizedExperiment
#' @description
#' `slice()` selects rows and/or columns by position.
#' Slice Documentation TO DO
#' @export
slice.PlySummarizedExperiment <- function(.data, ..., .preserve = FALSE) {
  plyxp(.data, slice_se_impl, ..., .preserve = .preserve)
}

slice_se_impl <- function(.data, ..., .preserve = FALSE) {
  # we will update .slice_data with some conviencence columns
  # then use that to slice the original data at the end
  # .slice_data <- .data
  .env <- caller_env()
  quos <- plyxp_quos(..., .ctx_default = "assays", .ctx_opt = c("rows", "cols"))

  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = "slice", review = err)
  }
  named_quos <- quos_is_named(quos = quos)
  if (any(named_quos)) {
    plyxp_should_not_named("slice")
  }
  # rename the quos so we can find them later
  names(quos) <- sprintf("..slice_%i", seq_along(quos))
  nms <- names(quos)
  quos <- quos_enforce_named(quos, nms)
  # to make this function consistent
  # groups <- group_data_se_impl(.data)
  # metadata(.data)[["group_data"]] <- NULL
  # rowData(.slice_data)[[".plyxp:::index"]] <- seq_len(nrow(.slice_data))
  # colData(.slice_data)[[".plyxp:::index"]] <- seq_len(ncol(.slice_data))
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "slice")
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  # we may have empty values, thus we do not collect with
  # mask$results, instead we will get the values raw and
  # handle the correct index
  chops <- mask$apply(
    .f = function(mask, name) {
      added <- mask$added
      names(added) <- added
      lapply(added, mask$get_chop)
    },
    .on_masks = c("rows", "cols")
  )
  chops <- flatten_slice_chops(chops, groups = group_data_se_impl(.data))
  which_ctx <- unique(ctxs)
  switch(length(which_ctx),
    `1` = {
      switch(which_ctx,
        rows = plyxp_slice_se(.data, chops$rows,
          .preserve = .preserve
        ),
        cols = plyxp_slice_se(.data, , chops$cols,
          .preserve = .preserve
        )
      )
    },
    `2` = {
      plyxp_slice_se(
        .data,
        chops$rows,
        chops$cols,
        .preserve = .preserve
      )
    }
  )
}

flatten_by_slice_expr <- function(ctx_chops, ctx_group) {
  if (is.null(ctx_group)) {
    # no groups - ctx_chops is a list off
    # index exprs
    do.call("c", ctx_chops)
  } else {
    # ctx_chops[[1]] --> expr1
    # ctx_chops[[2]] --> expr2 ...
    #
    # ctx_chops[[1]][...] --> expr1[group1, ..., groupn]
    chops <- lapply(ctx_chops,
      function(chop, true_ind) {
        Map(`[`, true_ind, chop)
      },
      true_ind = ctx_group$.indices
    )
    lapply(
      seq_along(ctx_group$.indices),
      function(i, chops) {
        do.call("c", lapply(chops, .subset2, i))
      },
      chops = chops
    )
  }
}

flatten_slice_chops <- function(chops, groups = NULL) {
  Map(
    flatten_by_slice_expr,
    ctx_chops = chops,
    ctx_group = groups %||% list(NULL, NULL)
  ) |>
    lapply(function(by_expr) {
      if (is.list(by_expr)) {
        by_expr <- do.call("c", by_expr)
      }
      if (!is.numeric(by_expr)) {
        stop("`slice` should resolve numeric slices")
      }
      by_expr
    })
}
