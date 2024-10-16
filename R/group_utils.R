
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
  n <- .nrow*.ncol
  out[[".nrows"]] <- map_int(out[[".rows::.indices"]], length)
  out[[".ncols"]] <- map_int(out[[".cols::.indices"]], length)
  attr(out, "row.names") <- c(NA_integer_, -n)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  
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
#   # browser()
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

#' @export
group_vars.SummarizedExperiment <- function(x) {
  map(metadata(x)[["group_data"]],
         function(x) {
           grep(x = names(x),
                pattern = "^.indices",
                value = TRUE,
                invert = TRUE)
         })
}



vec_chop_assays <- function(.data, .indices) {
  map2(
    attr(.indices, "plyxp:::row_chop_ind"),
    attr(.indices, "plyxp:::col_chop_ind"),
    function(.x, .y, .data) .data[.x, .y, drop = FALSE], .data = .data
  )
}

vec_chop_assays_row <- function(.data, .indices) {
  map(attr(.indices, "plyxp:::row_chop_ind"),
      function(.i, .data) .data[.i,,drop = FALSE],
      .data = .data)
}

vec_chop_assays_col <- function(.data, .indices) {
  map(attr(.indices, "plyxp:::col_chop_ind"),
      function(.i, .data) .data[,.i,drop = FALSE],
      .data = .data)
}


create_groups <- function(.data, .rename = ".indices") {
  # check if length > 0
  if (is_empty(.data)) return(NULL)
  # check first index has length > 0
  # assumes all others have similar length (probably not always true)
  if (length(.data[[1]])==0) {
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
  attr(out, "type") <- type
  if (type=="") return(NULL)
  out
}

get_group_indices <- function(
    .groups,
    .details,
    type = c("assays", "rowData", "colData")) {
  if (is.null(attr(.groups, "type"))) return(NULL)
  type <- match.arg(type, c("assays", "rowData", "colData"))
  switch(
    type,
    assays = {
      # browser()
      out <- map2(
        .details[[".rows::.indices"]],
        .details[[".cols::.indices"]],
        .f = function(row, col, n) {
          mat_index(row, col, nrows = n)
        }, n = attr(.groups, "obj_dim")[1])
      attr(out, "plyxp:::row_chop_ind") <- .details[[".rows::.indices"]]
      attr(out, "plyxp:::col_chop_ind") <- .details[[".cols::.indices"]]
      attr(out, "type") <- attr(.groups, "type")
      out},
    rowData = .groups$row_groups$.indices,
    colData = .groups$col_groups$.indices
  )
}

group_type <- function(obj) {
  result <- attr(obj, "type")
  if (is.null(result)) return("none")
  result
}

`group_type<-` <- function(obj, value) {
  value <- match.arg(value, choices = c("rowcol", "row", "col"))
  attr(obj, "type") <- value
  obj
}

group_details <- function(obj) {
  group_data <- metadata(obj)[["group_data"]]
  group_data$row_groups <- group_data$row_groups %||% tibble(.indices = list(seq_len(nrow(obj))), .indices_group_id = 1L)
  group_data$col_groups <- group_data$col_groups %||% tibble(.indices = list(seq_len(ncol(obj))), .indices_group_id = 1L)
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