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

obj_class <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "S7_object")) {
    return(S7::S7_class(x))
  }
  cls <- class(x)
  if (isS4(x)) {
    return(methods::getClass(cls))
  }
  cls
}

obj_class_name <- function(x) {
  cls <- obj_class(x)
  if (is.null(x)) {
    return("NULL")
  }
  if (inherits(x, "classRepresentation")) {
    return(paste0("S4<", x@package, x@className, ">"))
  }
  if (inherits(x, "S7_class")) {
    return(paste0("S7<", x@package, x@class_name, ">"))
  }
  paste0("S3<", paste(cls, collapse = "/"), ">")
}
