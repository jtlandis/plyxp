#' @title Create Group Indices
#' @name vec_group_id
#' @description
#' A alternative to [`vctrs::vec_group_id`][vctrs::vec_group_id] and
#' [`vctrs::vec_group_loc`][vctrs::vec_group_loc] as
#' an S4 generic function to allow `S4Vectors`.
#' @param x a vctrs vector or an S4Vector
#' @param ... unsued arguments
#' @return either a tibble or DataFrame object
#' @examples
#'
#' data <- S4Vectors::DataFrame(
#'   letter = sample(letters, 500, TRUE),
#'   LETTER = sample(letters, 500, TRUE)
#' )
#'
#' vec_group_id(data)
#' vec_group_loc(data)
#'
#' @export
vec_group_id <- new_generic(
  "vec_group_id",
  "x"
)

method(
  vec_group_id,
  class_vctrs
) <- function(x, ...) {
  vctrs::vec_group_id(x = x)
}

method(
  vec_group_id,
  class_s4_vctrs
) <- function(x, ...) {
  as(BiocGenerics::match(x, BiocGenerics::unique(x)), "integer")
}

df_vec_group_id <- function(df) {
  lapply(df@listData, vec_group_id) |>
    df0(nrows = df@nrows) |>
    vctrs::vec_group_id()
}

method(
  vec_group_id,
  class_DF
) <- function(x, ...) {
  df_vec_group_id(x)
}


## -----

#' @rdname vec_group_id
#' @export
vec_group_loc <- new_generic(
  "vec_group_loc",
  "x"
)

method(
  vec_group_loc,
  class_vctrs
) <- function(x, ...) {
  vctrs::vec_group_loc(x = x)
}

method(
  vec_group_loc,
  class_s4_vctrs
) <- function(x, ...) {
  # bioc_group_id is based on the object `x` and NOT
  # the mcols data. So for any arbitrary class `x` may
  # be, we do not consider the elementMetadata
  #
  # `Vector` extends Annotated, thus `mcols(x)` exists
  S4Vectors::mcols(x) <- NULL
  id <- vec_group_id(x)
  loc <- length(x) |>
    seq_len() |>
    split(f = id) |>
    unname()

  S4Vectors::DataFrame(
    key = S4Vectors::I(vec_slice(x, vapply(loc, `[`, 1L, 1L))),
    loc = S4Vectors::I(loc)
  )
}


method(
  vec_group_loc,
  class_DF
) <- function(x, ...) {
  out <- df_vec_group_id(x)
  out <- unname(split(seq_len(nrow(x)), out))
  S4Vectors::DataFrame(
    key = S4Vectors::I(vec_slice(x, vapply(out, `[`, 1L, 1L))),
    loc = S4Vectors::I(out)
  )
}

# proxies unlistData with vec_group_id and splits
# this result into a list paritioned by each group.
# key -> A list. Each element is a set of proxy vec_group_id of unlistData
# loc -> which indices of cmp dictate this group.
proxy_compress <- function(cmp) {
  ids <- rep(seq_len(vec_size(cmp)), lengths(cmp))
  uids <- vec_group_id(unlist(cmp))
  res <- split(uids, ids) |> vec_group_loc()
  res
}

method(
  vec_group_loc,
  class_CompressedList
) <- function(x, ...) {
  proxy <- proxy_compress(cmp = x)
  S4Vectors::DataFrame(
    key = S4Vectors::I(vec_slice(x, vapply(proxy$loc, `[`, 1L, 1L))),
    loc = S4Vectors::I(proxy$loc)
  )
}

method(
  vec_group_id,
  class_CompressedList
) <- function(x, ...) {
  proxy <- proxy_compress(cmp = x)
  loc <- proxy$loc
  ints <- structure(integer(length(x)), n = length(loc))
  for (i in seq_along(loc)) {
    ints[loc[[i]]] <- i
  }
  ints
}

# #' create groups
# #' @export
# create_groups <- function(.data, .rename = ".indices") {
#   #
#   # check if length > 0
#   if (is_empty(.data)) {
#     return(NULL)
#   }
#   # check first index has length > 0
#   # assumes all others have similar length (probably not always true)
#   if (length(.data[[1]]) == 0) {
#     .data <- tibble::as_tibble(.data)
#     .data[[.rename]] <- list()
#     .data[[sprintf("%s_group_id", .rename)]] <- integer()
#     return(.data)
#   }
#   grouped <- bioc_group_loc(.data)
#   .data <- c(
#     as.list(grouped$key),
#     setNames(list(grouped$loc), .rename),
#     setNames(list(seq_len(length(grouped$loc))), sprintf("%s_group_id", .rename))
#   )
#   # .data <- as(grouped$key, "DataFrame")
#   # .data[[.rename]] <- grouped$loc
#   # .data[[sprintf("%s_group_id", .rename)]] <- seq_len(length(grouped$loc))
#   as_DF(.data)
# }
