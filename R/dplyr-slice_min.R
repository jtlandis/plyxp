#' @name slice_extras
#' @title Extra slice functions for a PlySummarizedExperiment
#' @description
#' Convenience wrappers around [slice()] that select rows and/or columns of a
#' `PlySummarizedExperiment` object:
#'
#' * `slice_min()` and `slice_max()` select the rows or columns with the
#'   smallest or largest values of a variable.
#' * `slice_head()` and `slice_tail()` select the first or last rows or
#'   columns.
#' * `slice_sample()` randomly samples rows and/or columns, optionally
#'   weighted by a variable.
#'
#' Unlike their data.frame counterparts, every one of these verbs uses the
#' `rows()` and `cols()` context functions within `...` to declare which
#' dimension(s) of the object are sliced. If an object is grouped, slicing
#' happens within each group of the respective context.
#'
#' @param .data a `PlySummarizedExperiment` object.
#' @param ... calls to the `rows()` and/or `cols()` context functions. The
#'   `assays()` context is not available for slicing. The meaning of the
#'   context contents depends on the verb:
#'   * `slice_min()`/`slice_max()`: an expression giving the variable to order
#'     by within that context, e.g. `rows(length)`.
#'   * `slice_head()`/`slice_tail()`: must be **empty**, e.g. `rows()` or
#'     `cols()`. They only indicate which context(s) to slice.
#'   * `slice_sample()`: either empty, indicating a uniform sample of that
#'     context, or an expression, which is used as the `weight_by` value for
#'     that context.
#' @param n,prop Provide either `n`, the number of rows/columns, or `prop`, the
#'   proportion of rows/columns to select (per group). If neither is supplied,
#'   `n = 1` is used. Supplying both is an error.
#'
#'   A negative value of `n` or `prop` is subtracted from the group size,
#'   i.e. `n = -2` keeps all but 2 rows/columns. Positive values are rounded
#'   down, negative values rounded up, and the result is truncated to the group
#'   size (unless `slice_sample(replace = TRUE)`).
#'
#'   Always pass `n`/`prop` by name. Some calls error if they are left
#'   unnamed or omitted; see the section "Naming `n` and `prop`" below.
#' @param with_ties Should ties be kept together? The default, `TRUE`, may
#'   return more rows/columns than requested. Use `FALSE` to return exactly
#'   `n` rows/columns, with ties broken by position.
#' @param na_rm Should missing values be removed? The default, `FALSE`,
#'   ranks `NA` values last, so they are only kept when there are not enough
#'   non-missing values to satisfy `n`/`prop`. If `TRUE`, missing values are
#'   never selected.
#' @param weight_by Sampling weights wrapped in a context function, such as
#'   `rows(foo)` or `cols(bar)`. Each value must be non-negative. This is an
#'   alternative to giving a non-empty context in `...`; only one weight may be
#'   specified per context.
#' @param replace Should sampling be performed with (`TRUE`) or without
#'   (`FALSE`, the default) replacement? When `TRUE`, `n`/`prop` may exceed the
#'   group size.
#'
#' @details
#' ## Naming `n` and `prop`
#'
#' Before dispatching to the methods, the dplyr generics check that `n` was
#' not passed by position. If neither `n` nor `prop` is supplied, they error
#' when exactly one unnamed argument is left in `...`. For
#' `slice_min()`/`slice_max()`, the first context fills the generic's
#' `order_by` argument. As a result, either the `rows()` or the `cols()`
#' context can be used on its own, but they can only be used together when
#' `n` or `prop` is named. In other words, the following does **not** work:
#'
#' ```r
#' slice_min(xp, rows(foo), cols(bar))
#' ```
#'
#' but either of the following does:
#'
#' ```r
#' slice_min(xp, rows(foo), cols(bar), n = 1)
#'
#' xp |>
#'   slice_min(rows(foo)) |>
#'   slice_min(cols(bar))
#' ```
#'
#' Likewise, `slice_head()`, `slice_tail()` and `slice_sample()` need `n` or
#' `prop` to be named when only a single context is supplied, e.g.
#' `slice_head(xp, rows(), n = 1)` rather than `slice_head(xp, rows())`.
#'
#' ## `slice_head()`/`slice_tail()`
#'
#' For a data.frame, these generics do not use the `...` argument. For
#' `PlySummarizedExperiment` objects, `...` specifies the `rows()` and/or
#' `cols()` contexts to slice, and these expressions must be empty:
#'
#' ```r
#' slice_head(xp, rows(), n = 1)
#' slice_tail(xp, cols(), n = 1)
#' slice_head(xp, rows(), cols(), n = 2)
#' ```
#'
#' ## `slice_sample()`
#'
#' As with `slice_head()`/`slice_tail()`, empty context calls indicate which
#' contexts to sample. A non-empty context is used as the `weight_by` argument
#' for that context:
#'
#' ```r
#' # uniform sample of both
#' slice_sample(xp, rows(), cols(), n = 2)
#' # weight rows by `foo`, and cols by `bar`
#' slice_sample(xp, rows(foo), cols(bar), n = 2)
#' # weight rows by `foo`, uniform cols
#' slice_sample(xp, rows(foo), cols(), n = 2)
#' # weight rows by `foo`, using the `weight_by` argument
#' slice_sample(xp, weight_by = rows(foo), n = 2)
#' ```
#'
#' @return a `PlySummarizedExperiment` object.
#' @seealso [slice()], [dplyr::slice_min()], [dplyr::slice_head()],
#'   [dplyr::slice_sample()]
#' @aliases slice_min slice_max slice_head slice_tail slice_sample
#' @examples
#' # slice_min() / slice_max() ---------------------------------------
#' slice_max(se_simple, rows(length), n = 2)
#' slice_min(se_simple, cols(sample))
#' # name `n` when using both contexts
#' slice_min(se_simple, rows(length), cols(sample), n = 1)
#'
#' gse <- group_by(se_simple, rows(direction))
#' slice_max(gse, rows(length), n = 1)
#'
#' # slice_head() / slice_tail() -------------------------------------
#' slice_head(se_simple, rows(), n = 1)
#' slice_tail(se_simple, cols(), n = 2)
#' # negative `n` drops rows: all but the last 2
#' slice_head(se_simple, rows(), n = -2)
#' slice_head(se_simple, rows(), cols(), n = 2)
#'
#' # slice_sample() --------------------------------------------------
#' set.seed(1)
#' slice_sample(se_simple, rows(), cols(), n = 2)
#' slice_sample(se_simple, rows(length), cols(), n = 2)
#' slice_sample(se_simple, weight_by = rows(length), n = 2)
#' @export
slice_max.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  plyxp(
    .data,
    slice_max_se_impl,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm
  )
}

slice_max_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  slice_extreme_se_impl(
    .data,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
    extreme = "max"
  )
}

#' @rdname slice_extras
#' @export
slice_min.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  plyxp(
    .data,
    slice_min_se_impl,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm
  )
}

slice_min_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  with_ties = TRUE,
  na_rm = FALSE
) {
  slice_extreme_se_impl(
    .data,
    ...,
    n = n,
    prop = prop,
    with_ties = with_ties,
    na_rm = na_rm,
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

  size <- get_slice_size(n = n, prop = prop)

  quos <- plyxp_enquos(
    ...,
    .ctx = c("assays", "rows", "cols"),
  )

  quos <- lapply(
    quos,
    \(q) {
      plyxp_requo(
        q,
        expr = plyxp_slice_rank(
          !!q,
          size = size(`plyxp:::ctx:::n`),
          with_ties = !!with_ties,
          direction = !!direction,
          na_rm = !!na_rm
        )
      )
    }
  )
  slice_se_impl(.data, !!!quos, .fun = func)
}

#' @rdname slice_extras
#' @export
slice_head.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL
) {
  plyxp(
    .data,
    slice_head_se_impl,
    ...,
    n = n,
    prop = prop
  )
}

#' @rdname slice_extras
#' @export
slice_tail.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL
) {
  plyxp(
    .data,
    slice_tail_se_impl,
    ...,
    n = n,
    prop = prop
  )
}

slice_head_se_impl <- function(.data, ..., n = NULL, prop = NULL) {
  slice_n_se_impl(.data, ..., n = n, prop = prop, side = "head")
}

slice_tail_se_impl <- function(.data, ..., n = NULL, prop = NULL) {
  slice_n_se_impl(.data, ..., n = n, prop = prop, side = "tail")
}

slice_n_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  side = c("head", "tail")
) {
  side <- match.arg(side, c("head", "tail"))
  func <- sprintf("slice_%s", side)

  if (!is.null(n) && !is.null(prop)) {
    rlang::abort(
      sprintf("Cannot use both `n` and `prop` in `%s`.", func)
    )
  }
  if (is.null(n) && is.null(prop)) {
    n <- 1
  }

  size <- get_slice_size(n = n, prop = prop)

  quos <- plyxp_enquos(
    ...,
    .ctx = c("assays", "rows", "cols"),
    .exclude_empty = FALSE
  )

  if (
    length(quos) == 0 ||
      any(!vapply(quos, quo_is_missing, FALSE))
  ) {
    rlang::abort(
      sprintf(
        "All arguments to `%s` must be empty quosures, e.g. `rows()` or `cols()`.",
        func
      )
    )
  }

  idx <- switch(
    side,
    head = function(n, .size) {
      if (n < 1) {
        return(integer())
      }
      seq.int(1L, n)
    },
    tail = function(n, .size) {
      from <- .size - n + 1L
      if (.size < from) {
        return(integer())
      }
      seq.int(from, .size)
    }
  )

  quos <- lapply(
    quos,
    \(q) {
      plyxp_requo(
        q,
        expr = idx(size(`plyxp:::ctx:::n`), `plyxp:::ctx:::n`)
      )
    }
  )
  slice_se_impl(.data, !!!quos, .fun = func)
}


get_slice_size <- function(n, prop, allow_outsize = FALSE) {
  .type <- if (is.null(prop)) {
    "n"
  } else {
    "prop"
  }
  .size <- NULL
  input <- switch(
    .type,
    n = {
      body <- quote(input)
      as.numeric(n)
    },
    prop = {
      body <- quote(input * .size)
      as.numeric(prop)
    }
  )
  if (length(input) != 1L) {
    abort(sprintf("`%s` must be length 1", .type))
  }
  body <- if (input >= 0) {
    if (allow_outsize) {
      substitute(floor(body), list(body = body))
    } else {
      substitute(clamp(0, floor(body), .size), list(body = body))
    }
  } else {
    substitute(clamp(0, ceiling(.size + (body)), .size), list(body = body))
  }
  on.exit(rm(body, .size, .type))
  rlang::new_function(pairlist2(.size = ), body)
}

#' @rdname slice_extras
#' @export
slice_sample.PlySummarizedExperiment <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  weight_by = NULL,
  replace = FALSE
) {
  plyxp(
    .data,
    slice_sample_se_impl,
    ...,
    n = n,
    prop = prop,
    weight_by = enquo(weight_by),
    replace = replace
  )
}

slice_sample_se_impl <- function(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  weight_by = quo(NULL),
  replace = FALSE
) {
  func <- "slice_sample"

  if (!is.null(n) && !is.null(prop)) {
    rlang::abort(
      sprintf("Cannot use both `n` and `prop` in `%s`.", func)
    )
  }
  if (is.null(n) && is.null(prop)) {
    n <- 1
  }

  size <- get_slice_size(n = n, prop = prop, allow_outsize = replace)

  ctx <- c("assays", "rows", "cols")
  quos <- plyxp_enquos(..., .ctx = ctx, .exclude_empty = FALSE)

  # weight_by sugar -> plyxp quos
  wt_quos <- if (quo_is_null(weight_by)) {
    list()
  } else {
    construct_plyxp_quos(list(weight_by), .ctx = ctx, .exclude_empty = TRUE)
  }

  # merge wt_quos into quos by context
  quos <- c(quos, wt_quos)
  ctxs <- vapply(quos, plyxp_quo_ctx, FUN.VALUE = "", USE.NAMES = FALSE)
  squos <- split(quos, ctxs)
  quos <- Map(
    quos = squos,
    ctx = names(squos),
    f = function(quos, ctx) {
      switch(
        match(length(quos), 1, nomatch = 2L),
        set_missing_quo_null(quos[[1L]]),
        {
          # more than one quo...
          not_empty <- !vapply(quos, quo_is_missing, FALSE)
          n <- sum(not_empty)
          switch(
            match(n, c(0L, 1L), nomatch = 3L),
            set_missing_quo_null(quos[[1]]),
            quos[[which(not_empty)]],
            abort(c(
              "slice_sample cannot specify more than 1 weight per context",
              i = sprintf(
                "Detected %i variables within context `%s`",
                n,
                ctx
              )
            ))
          )
        }
      )
    }
  ) |>
    unlist(recursive = FALSE)

  quos <- lapply(quos, \(q) {
    plyxp_requo(
      q,
      expr = plyxp_slice_sample(
        `plyxp:::ctx:::n`,
        size(`plyxp:::ctx:::n`),
        replace = !!replace,
        weight = !!q
      )
    )
  })
  slice_se_impl(.data, !!!quos)
}

set_missing_quo_null <- function(quo) {
  if (quo_is_missing(quo)) {
    quo <- quo_set_expr(quo, expr(NULL))
  }
  quo
}


plyxp_slice_sample <- function(n, size, replace = FALSE, weight = NULL) {
  if (size == 0) {
    return(integer())
  } # sample.int errors on all-zero prob even at size 0
  if (!is.null(weight)) {
    l <- vec_size(weight)
    if (l != n) {
      abort(sprintf("weight must be size %i, not %i", n, l))
    }
  }
  sample.int(n, size, replace = replace, prob = weight)
}
