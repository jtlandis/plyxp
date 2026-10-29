check_quos_no_assays <- function(quos, fun, caller = caller_env()) {
  check_quos_no_ctx(
    quos = quos,
    ctx = "assays",
    fun = fun,
    caller = caller,
    message = c(i = "consider wrapping expressions in rows(...) or cols(...)")
  )
}

check_quos_no_ctx <- function(
  quos,
  ctx,
  fun,
  caller = caller_env(),
  message = character()
) {
  ctxs <- plyxp_quos_ctxs(quos)
  if (any(err <- ctxs %in% ctx)) {
    message <- enquo(message)
    message <- rlang::eval_tidy(rlang::expr(cli::format_inline(
      !!message,
      .envir = rlang::quo_get_env(message)
    )))
    plyxp_cannot_do_ctx(
      do = fun,
      ctx = ctx,
      review = err,
      message = message,
      caller = caller
    )
  }
  invisible(quos)
}

plyxp_cannot_do_ctx <- function(
  do,
  ctx,
  review,
  message = character(),
  caller = caller_env()
) {
  cli::cli_abort(
    message = c(
      "Cannot {do} in { {sprintf('`%s`', ctx)}} context{?s}",
      "x" = "review expression{?s}: { {sprintf('..%i', which(review))}}",
      message
    ),
    call = caller
  )
}

check_quos_not_named <- function(quos, fun, caller = caller_env()) {
  named_quos <- quos_is_named(quos = quos)
  if (any(named_quos)) {
    plyxp_should_not_named(fun, caller = caller)
  }
  invisible(quos)
}

plyxp_should_not_named <- function(fun, caller = caller_env()) {
  cli::cli_abort(
    message = c(
      "Expressions should not be named in {.fun {fun}}",
      "i" = "did you mean to use `==` instead of `=`?"
    ),
    call = caller
  )
}
