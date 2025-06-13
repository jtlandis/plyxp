
#' @title plyxp contexts
#' @name plyxp-context
#' @description
#' Contextual user-facing helper function for dplyr verbs with SummarizedExperiment
#' objects. These functions are intended to be used as the top level call to
#' any dplyr verbs `...` argument, similar to that of `across()`/`if_any()`/`if_all()`.
#' 
#' @param x,... expressions to evaluate within its associated context
#' @param asis asis = FALSE (the default) will indicate using active bindings
#' that attempt to coerce the underlying data into a format that is appropriate
#' for the current context. Indicating TRUE will instead bind the underlying data
#' as is.
#' @return function called for its side-effects
#' @examples
#' 
#' # cols
#' mutate(se_simple,
#'        cols(is_drug = condition=="drug"),
#'        #bind a different context
#'        effect = col_ctx(counts + (is_drug * rbinom(n(), 20, .3))))
#' 
NULL

#' @rdname plyxp-context 
#' @description
#' Specifies that the following expressions should be evaluated within the 
#' colData context.
#' @export
cols <- function(...) {
  abort("`cols()` is a sentinal function for SummarizedExperiment dplyr verbs")
}

#' @rdname plyxp-context 
#' @description
#' Specifies that the following expressions should be evaluated within the 
#' rowData context.
#' @export
rows <- function(...) {
  abort("`rows()` is a sentinal function for SummarizedExperiment dplyr verbs")
}


#' @rdname plyxp-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
col_ctx <- function(x, asis = FALSE) {
  env <- peek_ctx("plyxp:::caller_env")
  biocmanager <- peek_ctx("plyxp:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="cols") abort("`col_ctx()` within cols(...) is redunant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["cols"]]
  if (asis) {
    bot_env <- parent.env(bot_env)
  }
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}

#' @rdname plyxp-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
row_ctx <- function(x, asis = FALSE) {
  env <- peek_ctx("plyxp:::caller_env")
  biocmanager <- peek_ctx("plyxp:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="rows") abort("`row_ctx()` within rows(...) is redunant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["rows"]]
  if (asis) {
    bot_env <- parent.env(bot_env)
  }
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}

#' @rdname plyxp-context
#' @description
#' Specify a single expression to evaluate in another context
#' 
#' @export
assay_ctx <- function(x, asis = FALSE) {
  env <- peek_ctx("plyxp:::caller_env")
  biocmanager <- peek_ctx("plyxp:::manager")
  ctx <- biocmanager$ctx
  if (ctx=="assays") abort("`assay_ctx()` at top level ... is redundant")
  quo <- new_quosure(enexpr(x), env = env)
  bot_env <- biocmanager$extended[["assays"]]
  if (asis) {
    bot_env <- parent.env(bot_env)
  }
  mask <- new_data_mask(bot_env, top_env)
  eval_tidy(quo, data = mask, env = env)
}


#' @name dot-pronouns
#' @title contextual plyxp pronouns
#' @description
#' `plyxp` utilizes its own version of `rlang::.data` pronouns. These may be
#' used to gain access to other evaluation contexts for a managed set of 
#' data-masks.
#' 
#' Similar to `rlang::.data`, `plyxp::.assays` and other exported pronouns 
#' are exported to pass R CMD Checks. When using a `plyxp` within your package,
#' import the associated pronoun from `plyxp` but only use the fully unqualified 
#' name, `.assays`, `.assays_asis`, etc.
#' @return access to specific values behind the rlang pronoun
#' @examples
#' mutate(se_simple,
#'        # access via pronoun
#'        rows(sum = rowSums(.assays_asis$counts)),
#'        cols(sum = vapply(.assays$counts, sum, numeric(1))))
#' 
NULL

#' @rdname dot-pronouns
#' @export
.assays <- NULL

#' @rdname dot-pronouns
#' @export
.assays_asis <- NULL


#' @rdname dot-pronouns
#' @export
.rows <- NULL

#' @rdname dot-pronouns
#' @export
.rows_asis <- NULL

#' @rdname dot-pronouns
#' @export
.cols <- NULL

#' @rdname dot-pronouns
#' @export
.cols_asis <- NULL

# rows = function(...) {
#   # browser()
#   mask_manager <- peek_ctx("SE:::mask_manager")
#   fn <- peek_ctx("SE:::dplyr_function")
#   env <- peek_ctx("SE:::caller_env")
#   eval_fun <- switch(fn,
#                      mutate = mask_manager$eval_mutate_rows,
#                      group_by = mask_manager$eval_mutate_rows,
#                      stop(sprintf("`%s` is not yet implemented", fn)))
#   quos <- enquos(..., .named = TRUE)
#   nms <- names(quos)
#   for (i in seq_along(quos)) {
#     quo <- quo_set_env(quos[[i]], env)
#     name <- nms[i]
#     eval_fun(quo, name)
#   }
#   skip()
# },
# cols = function(...) {
#   mask_manager <- peek_ctx("SE:::mask_manager")
#   fn <- peek_ctx("SE:::dplyr_function")
#   env <- peek_ctx("SE:::caller_env")
#   eval_fun <- switch(fn,
#                      mutate = mask_manager$eval_mutate_cols,
#                      group_by = mask_manager$eval_mutate_cols,
#                      stop(sprintf("`%s` is not yet implemented", fn)))
#   quos <- enquos(..., .named = TRUE)
#   nms <- names(quos)
#   for (i in seq_along(quos)) {
#     quo <- quo_set_env(quos[[i]], env)
#     name <- nms[i]
#     eval_fun(quo, name)
#   }
#   skip()
# }