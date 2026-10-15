#' unused
# ctx_quo <- function(expr, env, ctx) {
#   quo <- new_quosure(expr = expr, env = env)
#   attr(quo, "plyxp:::ctx") <- ctx
#   quo
# }

# enforced named never branches to case where
# no names are not provided because we always force
# named quosures in `plyxp_quos()` through dots_list.
# consider dropping
enforce_named <- function(exprs) {
  to_update <- FALSE
  nms <- names(exprs) %||%
    {
      to_update <- TRUE
      vapply(exprs, rlang::as_label, "")
    }
  char_len <- nzchar(nms)
  if (any(to_rename <- char_len == 0)) {
    nms[to_rename] <- vapply(exprs[to_rename], rlang::as_label, "")
    to_update <- TRUE
  }
  if (to_update) {
    names(exprs) <- nms
  }
  exprs
}

as_trans_fn <- function(fn) {
  if (is_call(fn)) {
    if (identical(fn[[1]], quote(`function`))) {
      return(fn)
    }
  }
  if (is_function(fn)) {
    return(fn)
  }
  NULL
}

#' @title plyxp quosures
#' @description
#' a consistent way to handle `...` for dplyr extensions.
#' This returns a list of quosures where each quosure
#' contains an attribute `plyxp:::ctx` indicating which
#' mask context it should be evaluate in.
#' @param ... rlang dots, supports splicing an quoting
#' @param .named should resulting expressions be named?
#' @param .ctx A character string of availablecontexts. The first
#' element is considered to be the default context, whereas the
#' rest are considered optional.
#' @param .trans A list of functions to eventually apply to
#' the expressions depending on their context. The names of the list
#' elements should match the contexts names provided by `.ctx`. These
#' functions will be quoted and in-lined into the expressions and thus
#' cannot rely on the environments the functions are constructed.
#' These functions are quoted.
#' @param .exclude_empty Logical value. If TRUE, any empty expressions
#' are removed from the resulting list.
#' @return a quosure with attribute `plyxp:::ctx`.
#' @examples
#'
#' # in plyxp the default context is "assays"
#' # and optional contexts are "rows" and "cols"
#' quos <- plyxp_quos(
#'   foo = bar,
#'   ctx2(foo = bar),
#'   ctx3(foo = bar),
#'   .ctx_default = "ctx1",
#'   .ctx_opt = c("ctx2", "ctx3")
#' )
#' attr(quos[[1]], "plyxp:::ctx")
#' attr(quos[[2]], "plyxp:::ctx")
#' attr(quos[[3]], "plyxp:::ctx")
#'
#' @noRd
plyxp_quos <- function(
  ...,
  .named = TRUE,
  .ctx = NULL,
  .trans = list(),
  .exclude_empty = TRUE
) {
  #
  dots <- quos(...) |>
    as.list()
  if (is.null(.ctx)) {
    rlang::abort("`.ctx` must be specified!")
  }
  if (!is.list(.trans)) {
    rlang::abort("`.trans` must be a list!")
  }
  .trans <- lapply(.trans, as_trans_fn)
  .ctx_default <- .ctx[1] %||% rlang::abort("`.ctx` have at least 1 element!")
  .ctx_opt <- .ctx[-1]
  has_opt_ctx <- length(.ctx_opt) > 0
  # ctx_opt <- c("cols", "rows")
  nms <- rlang::names2(dots)
  is_nms <- nms != ""
  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    if (.exclude_empty && quo_is_missing(quo)) {
      # skip missing quosures
      next
    }
    .env <- quo_get_env(quo)
    .expr <- quo_get_expr(quo)
    if (has_opt_ctx && is_call(.expr, .ctx_opt)) {
      ctx <- as_label(.expr[[1]])
      ctx_exprs <- as.list(.expr[-1])
      # recapture the inner arguments allowing for dynamic
      # dots and using `"{foo}" := bar` notation
      ctx_exprs <- rlang::inject(rlang::exprs(!!!ctx_exprs), env = .env)
      ctx_nms <- rlang::names2(ctx_exprs)
      ctx_is_named <- ctx_nms != ""
      ctx_quos <- pmap(
        list(ctx_exprs, name = ctx_nms, is_named = ctx_is_named),
        plyxp_quo,
        env = .env,
        ctx = ctx,
        trans = .trans[[ctx]]
      )
      if (.exclude_empty) {
        # remove empty arguments from calls
        ctx_quos <- exclude_empty_quos(ctx_quos)
      } else if (is_empty(ctx_quos)) {
        # if we want to retain empty, and this context is empty...
        ctx_quos <- list(
          .empty = plyxp_quo(
            expr = expr(),
            env = base::emptyenv(),
            ctx = ctx,
            trans = .trans[[ctx]]
          )
        )
      }

      dots[[i]] <- splice(ctx_quos)
      next
    }

    dots[[i]] <- plyxp_quo(
      if (is_missing(.expr)) expr() else .expr,
      env = .env,
      ctx = .ctx_default,
      is_named = is_nms[i],
      name = nms[i],
      trans = .trans[[.ctx_default]]
    )
  }
  out <- do.call(dots_list, c(dots, list(.named = .named)))
  if (.exclude_empty) {
    out <- exclude_empty_quos(out)
  }
  if (.named) {
    # in case the prior expansion
    # of rows(...) and cols(...) need
    # a named argument
    out <- enforce_named(out)
  }
  out
}

is_empty_quo <- function(q) {
  is.null(q) || rlang::quo_is_missing(q)
}

exclude_empty_quos <- function(quos) {
  Filter(Negate(is_empty_quo), quos)
}

plyxp_quo <- function(expr, env, ctx, trans, ...) {
  quo <- new_quosure(expr = expr, env = env)
  attr(quo, "plyxp:::ctx") <- ctx
  attr(quo, "plyxp:::transform") <- trans
  attr(quo, "plyxp:::data") <- list2(...)
  quo
}


quos_is_named <- function(quos) {
  lapply(quos, attr, which = "plyxp:::data") |>
    vapply(`[[`, "is_named", FUN.VALUE = logical(1))
}

quos_enforce_named <- function(quos, names) {
  for (i in seq_along(quos)) {
    data <- attr(quos[[i]], "plyxp:::data")
    data$is_named <- TRUE
    data$name <- names[i]
    attr(quos[[i]], "plyxp:::data") <- data
  }
  quos
}
