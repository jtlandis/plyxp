# The following functions have a lot of boilerplate code

# whenever new binding in assays, ensure mask_rows has access.
connect_assays_to_rows <- function(mask_assays, mask_rows) {
  assay_names <- mask_assays$names
  size <- length(assay_names) + 20L
  env_mask_bind <- mask_rows$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  fun_asis <- add_bind(
    quote({
      # assumes !!name_sym is a matrix, if the user transforms a value in
      # assays to a non-matrix, and trys to access it, it may be incorrect
      chops <- `plyxp:::assays:::current_chops`
      data_chop <- .subset(!!name_sym, chops)
      as_is <- do.call("cbind", data_chop)
      if (length(chops) > 1) {
        col_ind <- attr(.indices, "plyxp:::col_chop_ind") |>
          .subset(chops)
        tryCatch(
          {
            as_is <- as_is[, order(list_unchop(col_ind)), drop = FALSE]
          },
          error = function(cnd) {
            if (!inherits(data_chop[[1]], "matrix")) {
              abort("could not reconstruct 'asis' representation. underlying data was not a matrix",
                call = NULL
              )
            }
            abort("unexpected error", parent = cnd, call = NULL)
          }
        )
      }
      as_is
    }),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_assays$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(lapply(seq_len(`plyxp:::ctx:::n`), function(i, x) x[i, , drop = TRUE], x = !!name_sym)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(assay_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new assay binding is made,
  # execute the prior functions
  mask_assays$on_bind(fun_asis)$on_bind(fun_mold)
  # allow assay data to be seen from rowData context
  mask_rows$environments@env_foreign_data |>
    env_bind(
      .assays_asis = as_data_pronoun(env_asis),
      .assays = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}
connect_assays_to_cols <- function(mask_assays, mask_cols) {
  assay_names <- mask_assays$names
  size <- length(assay_names) + 20L
  env_mask_bind <- mask_cols$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  fun_asis <- add_bind(
    quote({
      # assumes !!name_sym is a matrix, if the user transforms a value in
      # assays to a non-matrix, and trys to access it, it may be incorrect
      chops <- `plyxp:::assays:::current_chops`
      data_chop <- .subset(!!name_sym, chops)
      as_is <- do.call("rbind", data_chop)
      if (length(chops) > 1) {
        row_ind <- attr(.indices, "plyxp:::row_chop_ind") |>
          .subset(chops)
        tryCatch(
          {
            as_is <- as_is[order(list_unchop(row_ind)), , drop = FALSE]
          },
          error = function(cnd) {
            if (!inherits(data_chop[[1]], "matrix")) {
              abort("could not reconstruct 'asis' representation. underlying data was not a matrix",
                call = NULL
              )
            }
            abort("unexpected error", parent = cnd, call = NULL)
          }
        )
      }
      as_is
    }),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_assays$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(lapply(seq_len(`plyxp:::ctx:::n`), function(i, x) x[, i, drop = TRUE], x = !!name_sym)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(assay_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new assay binding is made,
  # execute the prior functions
  mask_assays$on_bind(fun_asis)$on_bind(fun_mold)
  # allow assay data to be seen from rowData context
  mask_cols$environments@env_foreign_data |>
    env_bind(
      .assays_asis = as_data_pronoun(env_asis),
      .assays = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_rows_to_assays <- function(mask_rows, mask_assays) {
  row_names <- mask_rows$names
  size <- length(row_names) + 20L
  env_mask_bind <- mask_assays$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # assays is always the "richer" grouping, we can guarantee
  # that the current_chops scope has a scalar value
  fun_asis <- add_bind(
    quote(.subset2(!!name_sym, `plyxp:::rows:::current_chops`)),
    # should be evaluated within the chopped context, cannot guarantee groupings
    .env_expr = mask_rows$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(!!name_sym, times = `plyxp:::ctx:::ncol`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(row_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new row binding is made,
  # execute the prior functions
  mask_rows$on_bind(fun_asis)$on_bind(fun_mold)
  # allow row data to be seen from assay context
  mask_assays$environments@env_foreign_data |>
    env_bind(
      .rows_asis = as_data_pronoun(env_asis),
      .rows = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_cols_to_assays <- function(mask_cols, mask_assays) {
  col_names <- mask_cols$names
  size <- length(col_names) + 20L
  env_mask_bind <- mask_assays$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # assays is always the "richer" grouping, we can guarantee
  # that the current_chops scope has a scalar value
  fun_asis <- add_bind(
    quote(.subset2(!!name_sym, `plyxp:::cols:::current_chops`)),
    .env_expr = mask_cols$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep_each(!!name_sym, times = `plyxp:::ctx:::nrow`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(col_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new col binding is made,
  # execute the prior functions
  mask_cols$on_bind(fun_asis)$on_bind(fun_mold)
  # allow col data to be seen from assay context
  mask_assays$environments@env_foreign_data |>
    env_bind(
      .cols_asis = as_data_pronoun(env_asis),
      .cols = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_rows_to_cols <- function(mask_rows, mask_cols) {
  row_names <- mask_rows$names
  size <- length(row_names) + 20L
  env_mask_bind <- mask_cols$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # bind
  fun_asis <- add_bind(
    # row data may be grouped. use vctrs::vec_c to concatenate vectors
    quote({
      # browser();vec_c(splice(.subset(!!name_sym, `plyxp:::rows:::current_chops`)))
      chops <- `plyxp:::rows:::current_chops`
      data_chop <- .subset(!!name_sym, chops)
      as_is <- vec_c(splice(data_chop))
      if (length(chops) > 1) {
        ind <- .subset(.indices, chops) |>
          splice() |>
          vec_c()
        as_is <- vec_slice(as_is, order(ind, method = "radix"))
      }
      as_is
    }),
    .env_expr = mask_rows$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(list(!!name_sym), times = `plyxp:::ctx:::n`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(row_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new row binding is made,
  # execute the prior functions
  mask_rows$on_bind(fun_asis)$on_bind(fun_mold)
  # allow row data to be seen from colData context
  mask_cols$environments@env_foreign_data |>
    env_bind(
      .rows_asis = as_data_pronoun(env_asis),
      .rows = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_cols_to_rows <- function(mask_rows, mask_cols) {
  col_names <- mask_cols$names
  size <- length(col_names) + 20L
  env_mask_bind <- mask_rows$environments[[1]] # last level
  env_asis <- new.env(hash = TRUE, parent = env_mask_bind, size = size)
  env_pronoun <- new.env(hash = TRUE, parent = env_asis, size = size)
  # bind
  fun_asis <- add_bind(
    # col data may be grouped. use vctrs::vec_c to concatenate vectors
    quote({
      # browser();vec_c(splice(.subset(!!name_sym, `plyxp:::cols:::current_chops`))))
      chops <- `plyxp:::cols:::current_chops`
      data_chop <- .subset(!!name_sym, chops)
      as_is <- vec_c(splice(data_chop))
      if (length(chops) > 1) {
        ind <- .subset(.indices, chops) |>
          splice() |>
          vec_c()
        as_is <- vec_slice(as_is, order(ind, method = "radix"))
      }
      as_is
    }),
    .env_expr = mask_cols$environments@env_data_chop,
    .env_bind = env_asis,
    type = "active"
  )
  fun_mold <- add_bind(
    quote(vec_rep(list(!!name_sym), times = `plyxp:::ctx:::n`)),
    .env_expr = env_asis,
    .env_bind = env_pronoun,
    type = "active"
  )
  walk(col_names, fun_asis) |>
    walk(fun_mold)
  # whenever a new col binding is made,
  # execute the prior functions
  mask_cols$on_bind(fun_asis)$on_bind(fun_mold)
  # allow col data to be seen from rowData context
  mask_rows$environments@env_foreign_data |>
    env_bind(
      .cols_asis = as_data_pronoun(env_asis),
      .cols = as_data_pronoun(env_pronoun)
    )
  invisible(env_pronoun)
}

connect_masks <- function(mask_assays, mask_rows, mask_cols) {
  col2row <- connect_cols_to_rows(mask_cols = mask_cols, mask_rows = mask_rows)
  row2col <- connect_rows_to_cols(mask_rows = mask_rows, mask_cols = mask_cols)
  col2assay <- connect_cols_to_assays(mask_cols = mask_cols, mask_assays = mask_assays)
  row2assay <- connect_rows_to_assays(mask_rows = mask_rows, mask_assays = mask_assays)
  assay2row <- connect_assays_to_rows(mask_assays = mask_assays, mask_rows = mask_rows)
  assay2col <- connect_assays_to_cols(mask_assays = mask_assays, mask_cols = mask_cols)

  list(
    assays = list(
      cols = col2assay,
      rows = row2assay
    ),
    rows = list(
      assays = assay2row,
      cols = col2row
    ),
    cols = list(
      assays = assay2col,
      rows = row2col
    )
  )
}
