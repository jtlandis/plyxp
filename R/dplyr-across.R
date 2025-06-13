#' @title expand across quosure
#' @description This function will transform a quosure
#' that uses the `across()` function and split it into a list
#' of quosures with the appropriate data to loop along. If the
#' quosure does not use `across()`, the quosure is returned as-is
#' in a list.
#' @param quo a quosure
#' @param ctx retrieves the current evaluation context of the quosure
#' @param mask the R6 mask-manager of all `rlang` data mask contexts
#' @param error_call function call to report there was an error if one is raised
#' @noRd
expand_across <- function(quo, ctx = attr(quo, "plyxp:::ctx"),
                          mask, error_call) {
  
  
  if (!quo_is_call(quo, "across", ns = c("", "dplyr"))) {
    return(list(quo))
  }
  # browser()
  env <- quo_get_env(quo)
  expr <- match.call(definition = dplyr::across,
                     call = quo_get_expr(quo),
                     expand.dots = FALSE,
                     envir = env)
  if (".cols" %in% names(expr)) {
    cols <- expr$.cols
  }
  else {
    # across_missing_cols_deprecate_warn()
    cols <- expr(everything())
  }
  cols <- as_quosure(cols, env)
  if (".fns" %in% names(expr)) {
    fns <- as_quosure(expr$.fns, env)
    # validate_fns <- dplyr:::quo_eval_fns
    fns <- validate_fns(fns,
                        mask = mask$masks[[ctx]]$environments[[1]],
                        error_call = error_call)
  }
  else {
    fns <- NULL
  }
  
  setup <- plyxp_across_setup(!!cols, fns = fns, names = eval(expr$.names),
                                 mask = mask$masks[[ctx]], manager = mask, 
                                 error_call = error_call, .caller_env = env,
                                 ctx = ctx)
  
  vars <- setup$vars
  fns <- setup$fns
  names <- setup$names
  
  n_vars <- length(vars)
  n_fns <- length(fns)
  seq_vars <- seq_len(n_vars)
  seq_fns <- seq_len(n_fns)
  expressions <- vector(mode = "list", n_vars * n_fns)
  columns <- character(n_vars * n_fns)
  k <- 1L
  for (i in seq_vars) {
    var <- vars[[i]]
    for (j in seq_fns) {
      var <- sym(var)
      fn <- fns[[j]]
      fn_call <- as_across_expr(fn, var)
      name <- names[[k]]
      expressions[[k]] <- plyxp_quo(fn_call, 
                                       ctx = ctx, env = env,
                                       name = name, 
                                       is_named = TRUE, 
                                       column = var)
      k <- k + 1L
    }
  }
  if (setup$ctx_swap) {
    ctx_swap <- switch (setup$ctx_swap,
      assays = expr(assay_ctx),
      rows = expr(row_ctx),
      cols = expr(col_ctx)
    )
    for (k in seq_along(expressions)) {
      quo <- expressions[[k]]
      expr_ <- quo_get_expr(quo)
      expressions[[k]] <- quo_set_expr(
        quo,
        expr((!!expr_) |> (!!ctx_swap)())
      )
    }
  }
  names(expressions) <- names
  expressions
  
  
}

#' @title setup across
#' @description
#' generate the list of new quosures based on the selected columns, functions
#' and returned names.
#' @param cols selected columns from across tidy-select
#' @param fns functions to apply to each selected column
#' @param names names to save the results into
#' @param mask specific data mask for evaluation context
#' @param ctx the evaluation context name
#' @param manager the R6 mask-manager
#' @param .caller_env environment the original quosure sure be evaluated within
#' @param error_call function call to report there was an error if one is raised
#' @noRd
plyxp_across_setup <- function(cols, fns, names, mask, ctx,
                                  manager, .caller_env, error_call) {
  cols <- enquo(cols)
  if (is.null(fns) && quo_is_call(cols, "~")) {
    bullets <- c("Must supply a column selection.", i = glue::glue("You most likely meant: `{across_if_fn}(everything(), {as_label(cols)})`."), 
                 i = "The first argument `.cols` selects a set of columns.", 
                 i = "The second argument `.fns` operates on each selected columns.")
    abort(bullets, call = error_call)
  }
  ## check if cols is contextual
  ## suppose we are in rows_ctx, but want
  ## to do something across the assays...
  ctx_swap <- FALSE
  if (quo_is_call(cols, c("assays","rows","cols"), n = 0)) {
    ctx_swap <- TRUE
    ctx <-  as_label(quo_get_expr(cols)[[1]])
    cols <- quo_set_expr(cols, expr(everything()))
  }
  
  if (identical(quo_get_expr(cols), quote(everything())) &&
      ctx %in% c("rows", "cols")) {
    # if identitcal AND we are in rows/cols context
    # explicitly change expr to remove either
    # .features/.samples. 
    # These should only be changed
    rm <- switch (ctx,
                  rows = expr(.features),
                  cols = expr(.samples))
    cols <- quo_set_expr(cols, expr(- (!!rm)))
  }
  
  data <- manager$masks[[ctx]]$ptype
  vars <- tidyselect::eval_select(cols, data = data, error_call = error_call)
  names_vars <- names(vars)
  vars <- names(data)[vars]
  # if (is.null(fns)) {
  #   if
  # }
  
  if (is.function(fns)) {
    names <- names %||% "{.col}"
    fns <- list(`1` = fns)
  }
  else {
    names <- names %||% "{.col}_{.fn}"
  }
  if (!is.list(fns)) {
    abort("Expected a list.", .internal = TRUE)
  }
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  }
  else {
    names_fns <- names2(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }
  
  glue_env <- env(.caller_env,
                  .col = vec_rep_each(names_vars, times = length(fns)),
                  .fn = vec_rep(names_fns, times = length(vars)))
  names <- vctrs::vec_as_names(glue::glue(names, .envir = glue_env),
                               repair = "check_unique",
                        call = error_call)
  list(vars = vars, fns = fns, names = names, ctx = ctx,
       ctx_swap = ctx_swap)
}

#' @title dplyr across internals
#' @name dplyr_across_internals
#' @description
#' the following functions were taken verbatim from the `dplyr` package such 
#' that behaviors of `across()` were consistent between `dplyr` and `plyxp`
#' @keywords internal 
#' @noRd
expr_substitute <- function(expr, old, new) 
{
  expr <- duplicate(expr)
  switch(typeof(expr),
         language = node_walk_replace(node_cdr(expr),old, new),
         symbol = if (identical(expr, old)) return(new))
  expr
}

# @rdname dplyr_across_internals
node_walk_replace <- function(node, old, new) {
  while (!is_null(node)) {
    switch(typeof(node_car(node)),
           language = {if (!is_call(node_car(node), c("~", "function")) || 
                          is_call(node_car(node), "~",n = 2)) 
             node_walk_replace(node_cdar(node), old, new)
             }, 
           symbol = if (identical(node_car(node), old)) node_poke_car(node, new))
    node <- node_cdr(node)
  }
}

# @rdname dplyr_across_internals
as_across_expr <- function(fn, var) {
  if (is_inlinable_lambda(fn)) {
    arg <- names(formals(fn))[[1]]
    expr <- body(fn)
    expr_substitute(expr, sym(arg), sym(var))
  }
  else {
    call2(fn, sym(var))
  }
}

# @rdname dplyr_across_internals
is_inlinable_lambda <- function(x) {
  is_function(x) && identical(fn_env(x), empty_env())
}

# @rdname dplyr_across_internals
validate_fns <- function(quo, mask, error_call = caller_env()) {
  sentinel_env <- empty_env()
  this <- current_env()
  out <- eval_tidy(quo({
    this$sentinel_env <- current_env()
    !!quo
  }))
  validate <- function(x) {
    if (is_formula(x) || is_function(x)) {
      if (identical(get_env(x), sentinel_env)) {
        if (is_inlinable_function(x)) {
          return(set_env(x, empty_env()))
        }
        if (is_inlinable_formula(x)) {
          x <- expr_substitute(x, quote(.), quote(.x))
          fn <- new_function(pairlist2(.x = ), f_rhs(x), 
                             empty_env())
          return(fn)
        }
        x <- set_env(x, mask)
      }
      as_function(x, arg = ".fns", call = error_call)
    }
    else {
      abort("`.fns` must be a function, a formula, or a list of functions/formulas.", 
            call = error_call)
    }
  }
  if (obj_is_list(out)) {
    map(out, function(elt) validate(elt))
  }
  else {
    validate(out)
  }
}

# @rdname dplyr_across_internals
is_inlinable_function <- function(x) 
{
  if (!is_function(x)) {
    return(FALSE)
  }
  fmls <- formals(x)
  if (length(fmls) != 1) {
    return(FALSE)
  }
  if ("return" %in% all.names(body(x))) {
    return(FALSE)
  }
  TRUE
}

# @rdname dplyr_across_internals
is_inlinable_formula <- function(x) {
  if (!is_formula(x, lhs = FALSE)) {
    return(FALSE)
  }
  nms <- all.names(x)
  unsupported_arg_rx <- "\\.\\.[0-9]|\\.y"
  if (any(grepl(unsupported_arg_rx, nms))) {
    return(FALSE)
  }
  if ("return" %in% nms) {
    return(FALSE)
  }
  TRUE
}
