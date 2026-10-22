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

plyxp_rank <- function(
  x,
  ties = c("min", "max", "first", "last"),
  direction = c("asc", "des"),
  na_value = c("largest", "smallest")
) {
  direction <- match.arg(direction, c("asc", "des"))
  na_value <- match.arg(na_value, c("largest", "smallest"))
  ties.method <- match.arg(ties, c("min", "max", "first", "last"))
  na.last <- switch(
    na_value,
    largest = TRUE,
    smallest = FALSE
  )
  if (direction == "des") {
    x <- -xtfrm(x)
    na.last <- !na.last
  }
  rank(x = x, na.last = na.last, ties.method = ties.method)
}

plyxp_slice_rank <- function(
  order_by,
  size,
  with_ties = TRUE,
  direction = c("asc", "des"),
  na_rm = FALSE
) {
  direction <- match.arg(direction, c("asc", "des"))
  na_value <- switch(direction, asc = "largest", des = "smallest")
  ties <- if (with_ties) {
    "min"
  } else {
    "first"
  }
  ranks <- plyxp_rank(
    x = order_by,
    ties = ties,
    direction = direction,
    na_value = na_value
  )
  keep <- ranks <= size
  if (na_rm) {
    keep[is.na(order_by)] <- FALSE
  }
  which <- which(keep)
  which[order(ranks[which])]
}

map_if <- function(.x, .p, .f, ...) {
  lgl <- vapply(.x, .p, logical(1))
  w <- which(lgl)
  if (length(w)) {
    .x[w] <- lapply(.x[w], .f, ...)
  }
  .x
}

# unreachable <- function(
#   message,
#   ...,
#   call = rlang::caller_call(),
#   parent = NULL,
#   report = FALSE
# ) {
#   frame <- parent.frame()
#   cli::cli_abort(
#     message = c(
#       "Unreachable code path reached",
#       message,
#       if (report) {
#         c(
#           i = "Please submit a bug report to {.href [the plyxp repository](https://github.com/jtlandis/plyxp/issues)}"
#         )
#       }
#     ),
#     class = "plyxp_unreachable",
#     call = call,
#     parent = parent,
#     .envir = frame
#   )
# }
