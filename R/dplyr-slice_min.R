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
#' @aliases slice_min slice_max
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

#' @rdname slice_extremes
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
          size = size(`plyxp:::ctx:::n`),
          with_ties = !!with_ties,
          direction = !!direction,
          na_rm = !!na_rm
        )
      )
    }
  )
  slice_se_impl(.data, !!!quos)
}

#' @rdname slice_extremes
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

#' @rdname slice_extremes
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

  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  if (any(err <- ctxs %in% "assays")) {
    plyxp_assays_cannot(do = func, review = err)
  }

  if (
    length(quos) == 0 ||
      any(quos_is_named(quos)) ||
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
  slice_se_impl(.data, !!!quos)
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

#' @rdname slice_extremes
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
