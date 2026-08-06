#' @importFrom dplyr mutate

#' @name mutate
#' @title Mutate a PlySummarizedExperiment object
#' @description
#' Mutate a PlySummarizedExperiment object under an data mask. Unlike a few other
#' `dplyr` implementations, all contextual evaluations of `mutate()` for
#' `SummarizedExperiment` are valid.
#' @param .data An object Inheriting from `PlySummarizedExperiment`, the wrapper
#' class for `SummarizedExperiment` objects
#' @param ... expressions to evaluate
#' @return an object inheriting PlySummarizedExperiment class
#' @examples
#'
#' mutate(se_simple,
#'   counts_1 = counts + 1,
#'   logp_counts = log(counts_1),
#'   # access assays context with ".assays" pronoun,
#'   # note that assays are sliced into a list to
#'   # fit dimensions of cols context
#'   cols(sum = purrr::map_dbl(.assays$counts, sum)),
#'   # access assays context "asis" with the same pronoun
#'   # but with a "_asis" suffix.
#'   rows(sum = rowSums(.assays_asis$counts))
#' )
#' @export
mutate.PlySummarizedExperiment <- function(.data, ...) {
  plyxp(.data, mutate_se_impl, ...)
}

mutate_se_impl <- function(.data, ...) {
  #
  .env <- caller_env()
  mask <- new_plyxp_manager.SummarizedExperiment(obj = .data)
  poke_ctx_local("plyxp:::caller_env", .env)
  poke_ctx_local("plyxp:::manager", mask)
  poke_ctx_local("plyxp:::dplyr_verb", "mutate")
  quos <- plyxp_quos(
    ...,
    .ctx = c("assays", "rows", "cols"),
    .trans = list(
      assays = quote(\(.data) {
        enforce_2d_size(
          .data,
          .nrow = `plyxp:::ctx:::nrow`,
          .ncol = `plyxp:::ctx:::ncol`
        )
      })
    )
  )
  ctxs <- vapply(quos, attr, FUN.VALUE = "", which = "plyxp:::ctx")
  nms <- names(quos)
  mask <- plyxp_evaluate(mask, quos, ctxs, nms, .env)
  results <- mask$results()

  nms <- names(results$rows)
  if (length(nms)) {
    if (".features" %in% nms) {
      rownames(.data) <- results$rows$.features
      results$rows$.features <- NULL
      nms <- names(results$rows)
    }
    for (i in seq_along(results$rows)) {
      rowData(.data)[[nms[i]]] <- results$rows[[i]]
    }
  }

  nms <- names(results$cols)
  if (length(nms)) {
    if (".samples" %in% nms) {
      colnames(.data) <- results$cols$.samples
      results$cols$.samples <- NULL
      nms <- names(results$cols)
    }
    for (i in seq_along(results$cols)) {
      colData(.data)[[nms[i]]] <- results$cols[[i]]
    }
  }
  # set assays last to reset dimnames
  nms <- names(results$assays)
  dim_nms <- dimnames(.data)
  for (i in seq_along(results$assays)) {
    new_assay <- results$assays[[i]]
    if (!is.null(new_assay)) {
      # could be removing in which we cannot set
      # this attrib
      dimnames(new_assay) <- dim_nms
    }
    assay(.data, nms[i], withDimnames = FALSE) <- new_assay
  }

  .data
}


enforce_2d_size <- function(
  .data,
  .nrow,
  .ncol
) {
  dims <- dim(.data)
  ndim <- length(dims)
  if (ndim >= 2L) {
    if (!all(dims[1L:2L] == c(.nrow, .ncol))) {
      stop(
        sprintf(
          "Assay dimensions (%i, %i) do not match expected dimensions (%i, %i)",
          dims[1L],
          dims[2L],
          .nrow,
          .ncol
        )
      )
    }
    return(.data)
  }
  n <- if (ndim == 0) vec_size(.data) else prod(dims)
  div <- 1L
  # if data is not scalar
  if (n != 1L) {
    p <- .nrow * .ncol
    r <- n %% p
    if (r != 0) {
      stop(
        sprintf(
          "Assay length (%i) is not a equal to or a multiple of expected dimensions (%i, %i)",
          n,
          .nrow,
          .ncol
        )
      )
    }
    div <- n / p
  }
  if (div == 1L) {
    matrix(.data, nrow = .nrow, ncol = .ncol)
  } else {
    array(.data, dim = c(.nrow, .ncol, div))
  }
}
