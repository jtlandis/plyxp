view_assays_from_rows <- biocmask::new_view_spec(
  ctx = "assays", from = "rows",
  .col_ind = unreplicate(.ctx$.indices$cols),
  .ncol_groups = length(.col_ind),
  .row_ind = unreplicate(.ctx$.indices$rows),
  .ordered_unchop = base::order(vctrs::list_unchop(base::as.list(.col_ind))),
  .nrow_groups = length(.row_ind),
  mapper = ~ ((seq_len(.ncol_groups) - 1L) * .nrow_groups
  ) + .x,
  asis = ~ {
    if (.ncol_groups < 2) {
      return(.subset2(.x, 1L))
    }
    .x <- do.call("cbind", .x)
    .x[, .ordered_unchop, drop = FALSE]
  },
  reshape = ~ lapply(seq_len(base::nrow(.x)), \(y) .x[y, , drop = FALSE])
)

view_rows_from_assays <- biocmask::new_view_spec(
  ctx = "rows", from = "assays",
  .col_ind = unreplicate(.indices$cols),
  .col_sizes = base::lengths(base::as.list(as.vector(.col_ind))),
  .ncol_groups = length(.col_ind),
  .row_ind = unreplicate(.indices$rows),
  .nrow_groups = length(.row_ind),
  mapper = ~ ((.x - 1L) %% .nrow_groups) + 1L,
  reshape = ~ bioc_rep(.x, times = .ncol_groups)
)


view_assays_from_cols <- biocmask::new_view_spec(
  ctx = "assays", from = "cols",
  .col_ind = unreplicate(.ctx$.indices$cols),
  .ncol_groups = length(.col_ind),
  .row_ind = unreplicate(.ctx$.indices$rows),
  .ordered_unchop = base::order(vctrs::list_unchop(base::as.list(.row_ind))),
  .nrow_groups = length(.row_ind),
  mapper = ~ seq_len(.nrow_groups) + .x - 1L,
  asis = ~ {
    if (.ncol_groups < 2) {
      return(.subset2(.x, 1L))
    }
    .x <- do.call("rbind", .x)
    .x[.ordered_unchop, , drop = FALSE]
  },
  reshape = ~ lapply(seq_len(base::ncol(.x)), \(y) .x[, y, drop = FALSE])
)


view_cols_from_assays <- biocmask::new_view_spec(
  ctx = "cols", from = "assays",
  .col_ind = unreplicate(.indices$cols),
  .ncol_groups = length(.col_ind),
  .row_ind = unreplicate(.indices$rows),
  .nrow_groups = length(.row_ind),
  mapper = ~ ((.x - 1L) %/% .nrow_groups) + 1L,
  reshape = ~ bioc_rep_each(.x, times = .nrow_groups)
)
