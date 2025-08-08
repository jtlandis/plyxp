slice.PlySummarizedExperiment <- function(.data, ..., .preserve = FALSE) {
  plyxp(.data, slice_se_impl, ..., .preserve = .preserve)
}

slice_se_impl <- function(.data, ..., .preserve = FALSE) {
  # we will update .slice_data with some conviencence columns
  # then use that to slice the original data at the end
  .slice_data <- .data
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
  rowData(.slice_data)[[".plyxp:::index"]] <- seq_len(nrow(.slice_data))
  colData(.slice_data)[[".plyxp:::index"]] <- seq_len(ncol(.slice_data))
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .slice_data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "slice")
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  `.plyxp:::index` <- slice_quos <- NULL
  which_ctx <- NULL
  if ("rows" %in% ctxs) {
    ri <- expr(c(!!!syms(nms[ctxs %in% "rows"])))
    slice_quos <- plyxp_quos(rows(`.plyxp:::index` = `.plyxp:::index`[!!ri]),
      .ctx_default = "assays", .ctx_opt = "rows"
    )
    which_ctx <- "rows"
  }
  if ("cols" %in% ctxs) {
    ci <- expr(c(!!!syms(nms[ctxs %in% "cols"])))
    slice_quos <- c(
      slice_quos,
      plyxp_quos(cols(`.plyxp:::index` = `.plyxp:::index`[!!ci]),
        .ctx_default = "assays", .ctx_opt = "cols"
      )
    )
    which_ctx <- c(which_ctx, "cols")
  }
  mask <- plyxp_evaluate(mask, slice_quos, which_ctx, names(slice_quos), .env)
  results <- mask$results()
  switch(length(which_ctx),
    `1` = {
      switch(which_ctx,
        rows = .data[results$rows[[".plyxp:::index"]], ],
        cols = .data[, results$cols[[".plyxp:::index"]]]
      )
    },
    `2` = {
      .data[results$rows[[".plyxp:::index"]], results$cols[[".plyxp:::index"]]]
    }
  )
}
