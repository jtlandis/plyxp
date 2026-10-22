#' @name slice_extremes
#' @title Slice maximum rows or columns of a PlySummarizedExperiment
#' @description
#' `slice_max()` selects rows or columns with the largest values of a variable.
#' `slice_min()` selects rows or columns with the smallest values of a variable.
#' @param .data a PlySummarizedExperiment object
#' @param ... expressions wrapped in `rows()` or `cols()` giving the ordering
#'   variable. Only the `rows()` and `cols()` contexts are available here.
#' @param n Number of rows/columns to select per group. Cannot be combined
#'   with `prop`.
#' @param prop Fraction of rows/columns to select per group. Cannot be
#'   combined with `n`.
#' @param with_ties Should ties be kept together? Default `TRUE`.
#' @param na_rm Should missing values be removed? Default `FALSE`.
#' @param .preserve When `FALSE` (default), grouping structure is recomputed
#'   based on the resulting data.
#' @aliases slice_min, slice_max
#' @examples
#' slice_max(se_simple, rows(length), n = 2)
#'
#' gse <- group_by(se_simple, rows(direction))
#' slice_max(gse, rows(length), n = 1)
#' @export
slice_max.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .preserve = FALSE
) {
  plyxp(
    .data,
    slice_max_se_impl,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
    .preserve = .preserve
  )
}

slice_max_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .preserve = FALSE
) {
  slice_extreme_se_impl(
    .data,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
    .preserve = .preserve,
    extreme = "max"
  )
}

#' @rdname slice_extremes
#' @export
slice_min.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .preserve = FALSE
) {
  plyxp(
    .data,
    slice_min_se_impl,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
    .preserve = .preserve
  )
}

slice_min_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .preserve = FALSE
) {
  slice_extreme_se_impl(
    .data,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
    .preserve = .preserve,
    extreme = "min"
  )
}


slice_extreme_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE,
  .preserve = FALSE,
  extreme = c("max", "min")
) {
  extreme <- match.arg(extreme, c("max", "min"))
  func <- sprintf("slice_%s", extreme)
  direction <- switch(
    extreme,
    max = "des",
    min = "asc"
  )
  if (!is.null(n) && !is.null(prop)) {
    rlang::abort("Cannot use both `n` and `prop` in `slice_max()`.")
  }
  if (is.null(n) && is.null(prop)) {
    n <- 1
  }

  size <- if (is.null(prop)) {
    n
  } else {
    prop <- vctrs::vec_cast(prop, double())
    vctrs::vec_assert(prop, double(), size = 1L, arg = "prop")
    stopifnot(prop >= 0 && prop <= 1)
    expr(.ctx(floor(!!prop * `plyxp:::ctx:::n`)))
  }

  quos <- plyxp_enquos(
    ...,
    .ctx = c("assays", "rows", "cols"),
  )

  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = func, review = err)
  }
  if (any(quos_is_named(quos))) {
    plyxp_should_not_named(func)
  }
  quos <- lapply(
    quos,
    \(q) {
      plyxp_requo(
        q,
        expr = plyxp_slice_rank(
          !!q,
          size = !!size,
          with_ties = !!with_ties,
          direction = !!direction,
          na_rm = !!na_rm
        )
      )
    }
  )
  slice_se_impl(.data, !!!quos, .preserve = .preserve)
}
