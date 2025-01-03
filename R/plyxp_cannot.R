plyxp_assays_cannot <- function(do, review) {
  cli::cli_abort(
    message = c(
      "Cannot {do} in `assays`",
      "x" = sprintf(
        "review expression: %s",
        paste0("..", which(review), collapse = ", ")
      ),
      "i" = "consider wrapping expressions in rows(...) or cols(...)"
    ),
    call = caller_env()
  )
}
