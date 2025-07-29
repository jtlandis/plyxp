##########
# UNUSED #
##########

# this function is likely going to be unused in the future. This was made to
# address a possible branch the codebase was possibly going towards, specifically
# for multi-grouped contexts and cross referencing issues.
# The NEW (and likely more stable assumption) is that if multigrouped context
# exists and we try to access colData from a row context, we pull the colData
# as-is, without being grouped.
case_key <- function(x, ...) {
  if (!inherits(x, "vctrs_grouped_list")) {
    cli::cli_abort(
      "x is not a {.cls vctrs_grouped_list}"
    )
  }
  group_keys <- attr(x, ".key")
  env <- current_env()
  dots <- list2(...)
  n_args <- length(dots)
  pairs <- map(dots, \(x) {
    list(lhs = f_lhs(x), rhs = f_rhs(x))
  })
  lhs_eval <- vector("list", n_args)
  rhs_eval <- vector("list", n_args)
  for (i in seq_along(dots)) {
    pair <- pairs[[i]]
    lhs_eval[[i]] <- eval_tidy(pair$lhs, env = env)
    rhs_eval[[i]] <- eval_tidy(pair$rhs, data = group_keys)
  }
  .size <- vec_size_common(!!!lhs_eval)
  conditions <- vec_recycle_common(!!!lhs_eval, .size = .size)
  seen <- logical(.size)
  out <- vector("list", .size)
  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    selection <- rhs_eval[[i]]
    adding <- (!seen) & cond
    out[adding] <- map2(x[adding], list(selection), ~ .x[.y])
    seen <- seen | adding
    if (all(seen)) break
  }

  if (all(map_int(out, length) == 1L)) {
    out <- unlist(out, recursive = FALSE)
  }
  out
}
