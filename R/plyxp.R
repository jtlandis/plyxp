expand_groups3 <- function(rows = NULL, cols = NULL, obj) {
  nr <- nrow(obj)
  nc <- ncol(obj)
  row_ind <- rows %||% IndexGrouping(indices = list(seq_len(nr)))
  col_ind <- cols %||% IndexGrouping(indices = list(seq_len(nc)))
  nr_grps <- NROW(row_ind)
  nc_grps <- NROW(col_ind)
  row_ind <- Replicated(row_ind, nc_grps)
  col_ind <- Replicated(col_ind, nr_grps, each = TRUE)
  if (nr_grps > 1 && nc_grps > 1) {
    rows <- row_ind
    cols <- col_ind
  } else {
    if (!is.null(rows)) rows <- S4Vectors::I(rows)
    if (!is.null(cols)) cols <- S4Vectors::I(cols)
  }
  indices <- map2(
    as.vector(row_ind)@indices,
    as.vector(col_ind)@indices,
    mat_index,
    nrows = nr
  )

  IndexGrouping(
    indices = indices,
    rows = rows,
    cols = cols
  )
}


#' @export
new_biocmask_manager.SummarizedExperiment <- function(obj, ...) {
  groups <- group_data_se_impl(obj)
  expanded <- expand_groups3(groups$row_groups, groups$col_groups)
  nr <- nrow(obj)
  nc <- ncol(obj)
  shared_ctx_env <- prepare_shared_ctx_env(groups = groups, expanded = expanded)

  mask_assay <- biocmask::new_biocmask(
    obj@assays,
    .indices = expanded, .top = top_env, .bot = bot_assay_env,
    .nrow = nr, .ncol = .nc
  )
  row_data <- if (methods::is(obj, "RangedSummarizedExperiment") &&
    require("plyranges")) {
    .gr <- SummarizedExperiment::rowRanges(obj)
    mcols(.gr) <- prepend_rownames(mcols(.gr), ".features")
    .gr
  } else {
    prepend_rownames(rowData(obj), ".features")
  }
  mask_rows <- biocmask::new_biocmask(
    row_data,
    .indices = expanded$cols, .top = top_env,
    .bot = biocmask::new_bioc_bot_env(context = "rows", parent = top_env)
  )

  mask_cols <- biocmask::new_biocmask(
    prepend_rownames(colData(obj), ".samples"),
    .indices = expanded$cols, .top = top_env,
    .bot = biocmask::new_bioc_bot_env(context = "cols", parent = top_env)
  )

  extended_environments <- connect_masks(
    mask_assays = mask_assay,
    mask_rows = mask_rows,
    mask_cols = mask_cols
  )

  plyxp_manager$new(
    .data = obj,
    .masks = list(
      assays = mask_assay,
      rows = mask_rows,
      cols = mask_cols
    ),
    .ctx_env = shared_ctx_env,
    .extended_env = extended_environments
  )
}

plyxp_evaluate <- function(mask, quos, ctxs, nams, env) {
  .call <- caller_call()
  # if (.matrix) {
  #   quos <- enforce_matrix(quos, ctxs)
  # }
  n_quo <- length(quos)
  curr_quo <- NULL
  try_fetch(
    {
      for (i in seq_len(n_quo)) {
        curr_quo <- quos[[i]]
        # nm <- nams[i]
        mask$ctx <- ctxs[[i]]
        mask$eval(curr_quo, env = env)
      }
    },
    error = function(cnd) {
      current_ctx <- mask$ctx
      current_gid <- mask$group_id
      message <- if (mask$n_groups > 1) {
        "error in expression {i} occured in group {current_gid} of the `{current_ctx}` context"
      } else {
        "error in expression {i} occured in the `{current_ctx}` context"
      }
      cli::cli_abort(
        message = message,
        parent = cnd,
        call = curr_quo,
        class = "plyxp_dplyr_eval_error"
      )
    }
  )
  invisible(mask)
}
