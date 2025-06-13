
as_vec <- function(x) {
  x <- as.vector(x)
  # unintuitively, a matrix of lists does NOT
  # become a vector after `as.vector()`,
  # must remove dim
  dim(x) <- NULL
  x
}


prepend_rownames <- function(DF, column) {
  name_col <- rownames(DF) %||% as.character(seq_len(nrow(DF)))
  name_col <- list(name_col)
  names(name_col) <- column
  DF@listData <- c(name_col, DF@listData)
  DF
}