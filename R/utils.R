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
    if (inherits(x, "S7_class")) {
      return(x)
    }
    return(S7::S7_class(x))
  }
  cls <- class(x)
  if (isS4(x)) {
    return(methods::getClass(cls))
  }
  cls
}

class_name_ <- function(cls) {
  if (is.null(cls)) {
    return("NULL")
  }
  if (inherits(cls, "classRepresentation")) {
    return(paste0("S4<", cls@package, "::", cls@className, ">"))
  }
  if (inherits(cls, "S7_class")) {
    return(paste0("S7<", cls@package, "::", cls@name, ">"))
  }
  if (!is.character(cls)) {
    stop("`cls` is not S4 or S7, expected character vector S3")
  }
  paste0("S3<", paste(cls, collapse = "/"), ">")
}

# obj_class_name <- function(x) {
#   cls <- obj_class(x)
#   class_name_(cls)
# }

assays_ <- function(x) {
  assays(x, withDimnames = FALSE)
}

`assays_<-` <- function(x, value) {
  assays(x, withDimnames = FALSE) <- value
  x
}
