
#' @title Create Group Indices
#' @name vec_group_id
#' @description
#' A alternative to [`vctrs::vec_group_id`][vctrs::vec_group_id] and
#' [`vctrs::vec_group_loc`][vctrs::vec_group_loc] as
#' an S4 generic function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_group_id
#' @return either a tibble or DataFrame object
#' @examples
#'
#' data <- DataFrame(
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


method(
  vec_group_id,
  class_DF
) <- function(x, ...) {
  lapply(x@listData, vec_group_id) |>
    as.data.frame() |>
    vctrs::vec_group_id()
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
    key = I(vec_slice(x, vapply(loc, `[`, 1L, 1L))),
    loc = I(loc)
  )
}


method(
  vec_group_loc,
  class_DF
) <- function(x, ...) {
  out <- lapply(x@listData, vec_group_id) |>
    as.data.frame() |>
    vctrs::vec_group_loc()
  S4Vectors::DataFrame(
    key = I(vec_slice(x, vapply(out$loc, `[`, 1L, 1L))),
    loc = I(out$loc)
  )
}


# #' create groups
# #' @export
# create_groups <- function(.data, .rename = ".indices") {
#   # browser()
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
