#' @name as.data.frame
#' @title create data.frame
#' @param x `SummarizedExperiment` object
#' @param ... unused arguments
#' @return a data.frame object
#' @examples
#' as.data.frame(se_simple)
#'
#' @export
as.data.frame.PlySummarizedExperiment <- function(x, ...) {
  x <- se(x)
  nc <- ncol(x)
  nr <- nrow(x)
  nn <- nc * nr
  .features <- rownames(x) %||% as.character(seq_len(nr))
  .samples <- colnames(x) %||% as.character(seq_len(nc))
  out <- c(
    list(
      .features = vec_rep(.features, times = nc),
      .samples = vec_rep_each(.samples, times = nr)
    ),
    lapply(assays(x), as_vec),
    lapply(rowData(x), vec_rep, times = nc),
    lapply(colData(x), vec_rep_each, times = nr)
  )
  attr(out, "row.names") <- c(NA_integer_, -nn)
  class(out) <- "data.frame"
  out
}
