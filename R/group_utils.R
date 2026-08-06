#' @importFrom dplyr bind_cols reframe across everything group_vars

# faster implementation
expand_groups2 <- function(.rows, .cols) {
  names(.rows) <- sprintf(".rows::%s", names(.rows))
  names(.cols) <- sprintf(".cols::%s", names(.cols))
  .nrow <- nrow(.rows)
  .ncol <- nrow(.cols)
  .rows <- map(.rows, vec_rep, times = .ncol)
  .cols <- map(.cols, vec_rep_each, times = .nrow)
  out <- c(.rows, .cols)
  n <- .nrow * .ncol
  out[[".nrows"]] <- map_int(out[[".rows::.indices"]], length)
  out[[".ncols"]] <- map_int(out[[".cols::.indices"]], length)
  attr(out, "row.names") <- c(NA_integer_, -n)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  attr(out, "plyxp:::unique_row_ind") <- seq_len(.nrow)
  attr(out, "plyxp:::unique_col_ind") <- ((seq_len(.ncol) - 1L) * .nrow) + 1L

  # due to this ordering here, I had introduced an unexpected
  # column-wise ordering of assays. I have changed it and commented
  # it out and also revered to the original intent of row-wise ordering.
  # o <- order(
  #   out[[".cols::.indices_group_id"]],
  #   out[[".rows::.indices_group_id"]]
  # )
  # out <- out[o,]
  out$.group_id <- seq_len(n)
  out
}

# inefficient and possibly defunct
# expand_groups <- function(.rows, .cols) {
#   #
#   .nrow <- nrow(.rows)
#   .ncol <- nrow(.cols)
#   bind_cols(
#     nest(
#       .rows,
#       .row_keys = -c(.indices, .indices_group_id)
#     ) |>
#       reframe(
#         across(
#           everything(),
#           ~vec_rep(.x, times = .env$.ncol)
#         ) |>
#           rename_with(.fn = \(x) gsub(".indices", ".rows", x = x))
#       ),
#     nest(
#       .cols,
#       .col_keys = -c(.indices, .indices_group_id)
#     ) |>
#       reframe(
#         across(
#           everything(),
#           ~vec_rep_each(.x, times = .env$.nrow)
#         ) |>
#           rename_with(.fn = \(x) gsub(".indices", ".cols", x = x))
#       ),
#     .name_repair = "minimal"
#   ) |>
#     arrange(
#       .rows_group_id,
#       .cols_group_id
#     ) |>
#     mutate(
#       .group_id = 1:n()
#     )
# }

mat_index <- function(rows_ind, cols_ind, nrows) {
  shift <- (cols_ind - 1L) * nrows
  vctrs::vec_rep(rows_ind, length(cols_ind)) +
    vctrs::vec_rep_each(shift, length(rows_ind))
}

is_grouped_rows <- function(.groups) {
  !is_empty(.groups$row_groups)
}

is_grouped_cols <- function(.groups) {
  !is_empty(.groups$col_groups)
}

#' @name group_vars
#' @title get PlySummarizedExperiment grouping Variables
#' @description
#' like in [dplyr::group_vars()] will get character strings for
#' groupings with the exection of the return value
#' being a list for each grouped context
#'
#' @param x PlySummarizedExperiment
#' @examples
#'
#' out <- group_by(se_simple, rows(direction))
#' group_vars(out)
#'
#' @return NULL or list containing names of grouping columns
#' @export
group_vars.PlySummarizedExperiment <- function(x) {
  group_vars_se_impl(se(x))
}

group_vars_se_impl <- function(x) {
  map(
    group_data_se_impl(x),
    function(x) {
      grep(
        x = names(x),
        pattern = "^.indices",
        value = TRUE,
        invert = TRUE
      )
    }
  )
}


into_dimlist <- function(assay_ind, ndims) {
  # we should always trust the groups sent to us,
  # using unique may change the expected number of
  # groups when constructing the object
  out <- rep(list(rlang::missing_arg()), ndims)
  row_chops <- attr(assay_ind, "plyxp:::row_chop_ind")
  if (!is.null(row_chops)) {
    out[[1L]] <- vctrs::vec_slice(
      row_chops,
      attr(assay_ind, "plyxp:::unique_row_ind")
    )
  }
  col_chops <- attr(assay_ind, "plyxp:::col_chop_ind")
  if (!is.null(col_chops)) {
    out[[2L]] <- vctrs::vec_slice(
      col_chops,
      attr(assay_ind, "plyxp:::unique_col_ind")
    )
  }
  out
}

chop_assays_outer <- function(obj, .ind) {
  dimlist <- into_dimlist(.ind, length(dim(obj)))
  chop_dims_outer(obj, dimlist)
}

chop_dims_outer <- function(obj, .dims) {
  # is_missing <- vapply(.dims, rlang::is_missing, FUN.VALUE = logical(1))
  n <- length(.dims)
  nlens <- lengths(.dims)
  out_size <- Reduce(`*`, nlens, right = TRUE, accumulate = 1L)

  curr_dim <- n
  obj_slice <- NULL
  objs <- out <- vector("list", out_size[[1L]])
  objs[[1L]] <- out[[1L]] <- obj
  n_obj <- 1L
  slice_expr <- expr(.slice)
  dim_args <- vec_rep(list(rlang::missing_arg()), n)
  while (curr_dim > 0) {
    .slices <- .dims[[curr_dim]]

    i_seq <- seq_len(nlens[curr_dim])
    nn <- length(i_seq)
    n_out <- out_size[curr_dim]
    out_seq <- seq_len(n_out)

    if (!is_missing(.slices)) {
      dim_args[[curr_dim]] <- slice_expr
      e <- inject(expr(obj_slice[!!!dim_args, drop = FALSE]))
      # out <- vector("list", out_size[curr_dim])
      for (j in seq_len(n_obj)) {
        obj_slice <- .subset2(objs, j)
        shift <- (j - 1L) * nn
        for (i in i_seq) {
          .slice <- .subset2(.slices, i)
          out[[shift + i]] <- eval(e)
        }
      }
      dim_args[[curr_dim]] <- missing_arg()
    }

    n_obj <- n_out
    curr_dim <- curr_dim - 1L
    objs[out_seq] <- out[out_seq]
  }
  out
}

vec_chop_assays <- function(.data, .indices) {
  map2(
    attr(.indices, "plyxp:::row_chop_ind"),
    attr(.indices, "plyxp:::col_chop_ind"),
    function(.x, .y, .data) .data[.x, .y, drop = FALSE],
    .data = .data
  )
}

vec_chop_assays_row <- function(.data, .indices) {
  map(
    attr(.indices, "plyxp:::row_chop_ind"),
    function(.i, .data) .data[.i, , drop = FALSE],
    .data = .data
  )
}

vec_chop_assays_col <- function(.data, .indices) {
  map(
    attr(.indices, "plyxp:::col_chop_ind"),
    function(.i, .data) .data[, .i, drop = FALSE],
    .data = .data
  )
}


create_groups <- function(.data, .rename = ".indices") {
  # check if length > 0
  if (is_empty(.data)) {
    return(NULL)
  }
  # check first index has length > 0
  # assumes all others have similar length (probably not always true)
  if (length(.data[[1]]) == 0) {
    .data <- as_tibble(.data)
    .data[[.rename]] <- list()
    .data[[sprintf("%s_group_id", .rename)]] <- integer()
    return(.data)
  }
  .data |>
    as_tibble() |>
    vec_group_loc() |>
    unnest(key) |>
    rename("{.rename}" := loc) |>
    mutate("{.rename}_group_id" := seq_len(n()))
}

plyxp_groups <- function(row_groups = NULL, col_groups = NULL) {
  out <- list(
    row_groups = create_groups(row_groups),
    col_groups = create_groups(col_groups)
  )
  type <- ""
  if (!is.null(out$row_groups)) {
    type <- "row"
  }
  if (!is.null(out$col_groups)) {
    type <- paste0(type, "col")
  }
  class(out) <- "plyxp_groups"
  # attr(out, "type") <- type
  if (type == "") {
    return(NULL)
  }
  out
}

get_group_indices <- function(
  .groups,
  .details,
  type = c("assays", "rowData", "colData")
) {
  if (is.null(.groups)) {
    return(NULL)
  }
  type <- match.arg(type, c("assays", "rowData", "colData"))
  switch(
    type,
    assays = {
      #
      out <- map2(
        .details[[".rows::.indices"]],
        .details[[".cols::.indices"]],
        .f = function(row, col, n) {
          mat_index(row, col, nrows = n)
        },
        n = attr(.groups, "obj_dim")[1]
      )
      attr(out, "plyxp:::row_chop_ind") <- .details[[".rows::.indices"]]
      attr(out, "plyxp:::col_chop_ind") <- .details[[".cols::.indices"]]
      attr(out, "plyxp:::unique_row_ind") <- attr(
        .details,
        "plyxp:::unique_row_ind"
      )
      attr(out, "plyxp:::unique_col_ind") <- attr(
        .details,
        "plyxp:::unique_col_ind"
      )
      # attr(out, "type") <- attr(.groups, "type")
      out
    },
    rowData = {
      if (isFALSE(attr(.groups, "grouped_rows"))) {
        NULL
      } else {
        .groups$row_groups$.indices
      }
    },
    colData = {
      if (isFALSE(attr(.groups, "grouped_cols"))) {
        NULL
      } else {
        .groups$col_groups$.indices
      }
    }
  )
}

# group_type <- function(obj) {
#   result <- attr(obj, "type")
#   if (is.null(result)) {
#     return("none")
#   }
#   result
# }

# `group_type<-` <- function(obj, value) {
#   value <- match.arg(value, choices = c("rowcol", "row", "col"))
#   attr(obj, "type") <- value
#   obj
# }

group_details <- function(obj) {
  group_data <- group_data_se_impl(obj)
  if (is.null(group_data$row_groups)) {
    group_data$row_groups <- tibble(
      .indices = list(seq_len(nrow(obj))),
      .indices_group_id = 1L
    )
    attr(group_data, "grouped_rows") <- FALSE
  }

  if (is.null(group_data$col_groups)) {
    group_data$col_groups <- tibble(
      .indices = list(seq_len(ncol(obj))),
      .indices_group_id = 1L
    )

    attr(group_data, "grouped_cols") <- FALSE
  }

  # out <- list(
  #   row_groups = row_groups,
  #   col_groups = col_groups
  # )
  # out <- expand_groups2(row_groups, col_groups)
  attr(group_data, "obj_dim") <- dim(obj)
  # attr(out, "type") <- attr(group_data, "type")
  # out |>
  #   mutate(
  #     .nrows = map_int(`.rows::.indices`, length),
  #     .ncols = map_int(`.cols::.indices`, length)
  #   )
  group_data
}

group_ind <- function(x, n) {
  int <- integer(n)
  for (i in seq_along(x)) {
    int[x[[i]]] <- i
  }
  int
}

slice_group_data <- function(groups, indices, .size, .preserve = FALSE) {
  if (is.null(groups)) {
    return(groups)
  }
  group_inds <- group_ind(groups$.indices, .size)
  new_id <- vctrs::vec_slice(group_inds, indices)
  new_grps <- vctrs::vec_group_loc(new_id)
  inds <- lapply(seq_len(nrow(groups)), function(i) integer())
  inds[new_grps$key] <- new_grps$loc
  groups$.indices <- inds
  if (!.preserve) {
    groups <- groups[new_grps$key, ]
    groups$.indices_group_id <- seq_len(nrow(groups))
  }
  groups
}


unchop_2d <- function(lst, row_ind, col_ind) {
  dims <- lapply(lst, dim)
  ndims <- vctrs::vec_size_common(!!!dims)
  total_size <- sum(vapply(dims, prod, FUN.VALUE = numeric(1)))
  nrow <- sum(vapply(dims[seq_along(row_ind)], `[`, FUN.VALUE = numeric(1), 1))
  col_slice <- (seq_along(col_ind) - 1L) * length(row_ind) + 1L
  ncol <- sum(vapply(dims[col_slice], `[`, FUN.VALUE = numeric(1), 2))
  dim_args <- c(
    list(rlang::expr(i)),
    list(rlang::expr(j)),
    rep(list(rlang::missing_arg()), ndims - 2L)
  )
  exprn <- expr(rlang::inject(out[!!!dim_args] <- lst[[k]]))
  out <- vector(typeof(lst[[1]]), total_size)
  out <- if (ndims == 2) {
    matrix(out, nrow = nrow, ncol = ncol)
  } else {
    array(out, dim = c(nrow, ncol, total_size / (nrow * ncol)))
  }
  k <- 0L
  for (j in col_ind) {
    for (i in row_ind) {
      k <- k + 1L
      eval(exprn)
    }
  }
  out
}
