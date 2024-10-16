
ctx_quo <- function(expr, env, ctx) {
  quo <- new_quosure(expr = expr, env = env)
  attr(quo, "biocmask:::ctx") <- ctx
  quo
}

enforce_named <- function(exprs) {
  to_update <- FALSE
  nms <- names(exprs) %||% {to_update <- TRUE; vapply(exprs, rlang::as_label, "")}
  char_len <- nzchar(nms)
  if (any(to_rename <- char_len==0)) {
    nms[to_rename] <- vapply(exprs[to_rename], rlang::as_label, "")
    to_update <- TRUE
  }
  if (to_update) {
    names(exprs) <- nms
  }
  exprs
}

#' @title biocmask quosures
#' @description
#' a consistent way to handle `...` for dplyr extensions.
#' This returns a list of quosures where each quosure 
#' contains an attribute `biocmask:::ctx` indicating which
#' mask context it should be evaluate in.
#' @param ... rlang dots, supports splicing an quoting
#' @param .named should resulting expressions be named?
#' @param .ctx_default default context to eval within
#' @param .ctx_opt optional contexts to eval within
#' @return a quosure with attribute `biocmask:::ctx`.
#' @examples
#' 
#' # in biocmask the default context is "assays"
#' # and optional contexts are "rows" and "cols"
#' quos <- biocmask_quos(
#'   foo = bar,
#'   ctx2(foo = bar),
#'   ctx3(foo = bar),
#'   .ctx_default = "ctx1",
#'   .ctx_opt = c("ctx2", "ctx3")
#' )
#' attr(quos[[1]], "biocmask:::ctx")
#' attr(quos[[2]], "biocmask:::ctx")
#' attr(quos[[3]], "biocmask:::ctx")
#' 
#' @export
biocmask_quos <- function(..., .named = TRUE, .ctx_default = NULL, .ctx_opt = NULL) {
  # browser()
  dots <- quos(...) |>
    as.list()
  .ctx_default <- .ctx_default %||% abort("`.ctx_default` must be specified!")
  has_opt_ctx <- !is.null(.ctx_opt)
  # ctx_opt <- c("cols", "rows")
  nms <- rlang::names2(dots)
  is_nms <- nms != ""
  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    .env <- quo_get_env(quo)
    .expr <- quo_get_expr(quo)
    if (has_opt_ctx && is_call(.expr, .ctx_opt)) {
      ctx <- as_label(.expr[[1]])
      ctx_exprs <- as.list(.expr[-1])
      ctx_nms <- rlang::names2(ctx_exprs)
      ctx_is_named <- ctx_nms != ""
      ctx_quos <- pmap(
        list(ctx_exprs,
             name = ctx_nms,
             is_named = ctx_is_named),
        biocmask_quo,
        env = .env,
        ctx = ctx)
      dots[[i]] <- splice(ctx_quos)
      next
    }
    
    dots[[i]] <- biocmask_quo(.expr, env = .env,
                              ctx = .ctx_default,
                              is_named = is_nms[i], 
                              name = nms[i])
  } 
  out <- do.call(dots_list, c(dots, list(.named = .named)))
  if (.named) {
    # in case the prior expansion
    # of rows(...) and cols(...) need
    # a named argument
    out <- enforce_named(out)
  }
  out
}

biocmask_quo <- function(expr, env, ctx, ...) {
  quo <- new_quosure(expr = expr, env = env)
  attr(quo, "biocmask:::ctx") <- ctx
  attr(quo, "biocmask:::data") <- list2(...)
  quo
}

enforce_matrix <- function(quos, ctxs) {
  is_assay_ctx <- ctxs == "assays"
  quos[is_assay_ctx] <- lapply(
    quos[is_assay_ctx],
    function(quo) {
      if (is.null(quo_get_expr(quo))) 
        return(quo)
      else
        quo_set_expr(quo,
                     expr(matrix(!!quo,
                                 nrow = `biocmask:::ctx:::nrow`,
                                 ncol = `biocmask:::ctx:::ncol`)))
      }
    )
  quos
}
