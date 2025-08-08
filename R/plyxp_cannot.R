plyxp_assays_cannot <- function(do, review) {
  cli::cli_abort(
    message = c(
      "Cannot {do} in `assays` context",
      "x" = sprintf(
        "review expression: %s",
        paste0("..", which(review), collapse = ", ")
      ),
      "i" = "consider wrapping expressions in rows(...) or cols(...)"
    ),
    call = caller_env()
  )
}


plyxp_should_not_named <- function(fun) {
  cli::cli_abort(
    message = c(
      "Expressions should not be named in {.fun {fun}}",
      "i" = "did you mean to use `==` instead of `=`?"
    ),
    call = caller_env()
  )
}
