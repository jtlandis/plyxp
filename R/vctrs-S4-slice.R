#' @title Get observations of a vector
#' @name vctrs_slice
#' @description
#' This extends `vctrs::vec_slice` to `S4Vectors::Vector` class by masking
#' `vec_slice` with `S7::new_generic`. Atomic vectors and other base S3 classes
#'  (list, data.frame, factor, Dat, POSIXct) will dispatch to the
#' `vctrs::vec_slice` method as normal. Dispatch support on the
#' `S4Vectors::Vector` and `S4Vectors::DataFrame` classes provides a unified
#' framework for working with base R vectors and `S4Vectors`.
#'
#' ### `S4Vectors::Vector` Implementation
#'
#' This method will naively call the `[` method for any S4 class that inherits
#' from the `S4Vectors::Vector` class. This may not be a very efficient way to
#' slice up an S4 class, but will work.
#'
#' With this implementation, the `x@mcol` data is expected to be retained after
#' a call to `plyxp::vec_slice(x, i)`.
#'
#' ### `S4Vectors::DataFrame` Implementation
#'
#' The `DataFrame` implementation works similar to how `vctrs::vec_slice` works
#' on a `data.frame` object. What is being sliced is the rows of `x@listData`.
#' To maintain the size stability of the `DataFrame` object, we change `@nrows`
#' to the appropriate value, and perform a recursive call if `@elementMetadata`
#' is not `NULL`.
#'
#' ## Performance
#'
#' Depending on the size and complexity of your S4 Vector object, you may find
#' the standard subset operation is extremely slow. For example, consider a
#' `SummarizedExperiment` whose rowData contains a `CompressedGRangesList`
#' object assigned to the name "exons" and whose length is 250,000 and
#' underlying `@unlistData` is length 1,600,000. Performing a by `.features`
#' grouping operation and attempting to evaluate the `exons` within the row
#' context would force the `CompressedGRangesList` object to be
#' chopped element-wise.
#'
#' Unfortunately, there is a massive performance hit in attempting to construct
#' 250,000 `GRanges`. Unless you do not mind waiting over an hour for each
#' `dplyr` verb in which `exons` gets evaluated, doing so  is not recommended.
#'
#' The `plyxp` package is planning to export a new generic
#' named `plyxp_s4_proxy_vec()`.
#' This attempts to reconstruct certain standard `S4Vectors::Vectors` as
#' standard vectors or tibbles. The equivalent `exons` object would require
#' much more memory use, but at the advantage of only taking several seconds to
#' construct. When you are done, you can attempt to restore the original S4
#' Vector with `plyxp_restore_s4_proxy()`.
#'
#' In development, `plyxp_s4_proxy_vec()` is faster to work with because there
#' are less checks on the object validity and all `@elementMetadata` and
#' `@metadata` are dropped from the objects.
#'
#' @inheritParams vctrs::vec_slice
#' @return a new S3 or S4 vector subsetted by `i`
#' @examples
#' vec_slice(1:10, i = 5)
#' vec_slice(S4Vectors::Rle(rep(1:3, each = 3)), i = 5)
#'
#' @export
vec_slice <- S7::new_generic(
  "vec_slice",
  dispatch_args = "x",
  function(x, i, ...) {
    S7_dispatch()
  }
)

method(vec_slice, class_vctrs) <- function(
  x,
  i,
  ...,
  error_call = current_env()
) {
  vctrs::vec_slice(x, i, ..., error_call = error_call)
}

method(vec_slice, getClass("CompressedGRangesList")) <- function(x, i, ...) {
  st <- IRanges::start(x@partitioning)
  en <- IRanges::end(x@partitioning)
  w <- IRanges::width(x@partitioning)[i]
  seqs <- purrr::map2(st[i], en[i], `:`)

  gr <- IRanges::extractROWS(x@unlistData, vctrs::vec_c(rlang::splice(seqs)))

  cgr <- as(gr, "CompressedGRangesList")
  S4Vectors::new2(
    "CompressedGRangesList",
    unlistData = gr,
    partitioning = IRanges::PartitioningByEnd(
      x = cumsum(w),
      names = names(x@partitioning)[i]
    ),
    elementType = "GRanges",
    elementMetadata = IRanges::extractROWS(x@elementMetadata, i),
    metadata = x@metadata
  )
}

method(vec_slice, class_vec_phantom) <- function(x, i, ...) {
  attr(x, "phantomData") <- vec_slice(attr(x, "phantomData"), i)
  x
}

method(vec_slice, S7::class_any) <- function(x, i, ...) {
  x[i]
}

method(vec_slice, class_s4_vctrs) <- function(x, i, ...) {
  x[i]
}

method(vec_slice, class_DF) <- function(x, i, ...) {
  x@listData <- purrr::map(x@listData, vec_slice, i = i)
  x@nrows <- length(i)
  if (!is.null(x@elementMetadata)) {
    x@elementMetadata <- Recall(x = x@elementMetadata, i = i)
  }
  x
}

# cgr_as_lst <- function(x) {
#   n <- length(x)
#   st <- IRanges::start(x@partitioning)
#   en <- IRanges::end(x@partitioning)
#   seqs <- purrr::map2(st, en, `:`)
#   ldata <- x@unlistData
#   box::use(vctrs[vec_chop], S4Vectors[new2])
#   C_obj <- methods:::C_new_object
#   IRANGE <- getClass("IRanges")
#   seqnames <- rep(ldata@seqnames@values, times = ldata@seqnames@lengths) |>
#     vec_chop(indices = seqs) |> purrr::map(S4Vectors::Rle, .progress = TRUE)
#   ranges_start <- ldata@ranges@start |> vec_chop(indices = seqs)
#   ranges_width <- ldata@ranges@width |> vec_chop(indices = seqs)
#   ranges_names <- ldata@ranges@NAMES |> vec_chop(indices = seqs)
#   iranges <- rep(list(.Call(C_obj, IRANGE)), times = length(ranges_start))
#   ranges <- purrr::pmap(
#     list(ranges_start, ranges_width, ranges_names, iranges),
#     function(st, wd, nm, obj) {
#       obj@start <- st
#       obj@width <- wd
#       names(obj) <- nm
#       obj
#     }, .progress = TRUE)
#   strand <- rep(ldata@strand@values, times = ldata@strand@lengths) |>
#     vec_chop(indices = seqs) |> purrr::map(S4Vectors::Rle, .progress = TRUE)
#   vslice <- vctrs::vec_slice
#   new_gr <- S4Vectors::new2
#   RLE <- S4Vectors::Rle
#   irange <- IRanges::IRanges
#   purrr::pmap(
#     list(
#       seqnames = seqnames,
#       ranges = ranges,
#       strand = strand
#     ),
#     function(seqnames, ranges,
#              strand) {
#       new_gr(
#         "GRanges",
#         seqnames = seqnames,
#         ranges = ranges,
#         strand = seqnames,
#         # elementMetadata = ldata@elementMetadata[i,],
#         seqinfo = ldata@seqinfo,
#         check = FALSE
#       )
#     },
#     .progress = T
#
#   )
#   # purrr::pmap(
#   #   seqs,
#   #   function(i, seqnames, ranges_start,
#   #            ranges_width, ranges_names,
#   #            strand) {
#   #     new_gr(
#   #       "GRanges",
#   #       seqnames = RLE(vslice(seqnames, i)),
#   #       ranges = irange(
#   #         start = vslice(ranges_start, i),
#   #         width = vslice(ranges_width, i),
#   #         names = vslice(ranges_names, i)
#   #       ),
#   #       strand = RLE(vslice(strand, i)),
#   #       elementMetadata = ldata@elementMetadata[i,],
#   #       seqinfo = ldata@seqinfo,
#   #       check = FALSE
#   #     )
#   #   },
#   #   seqnames = seqnames,
#   #   ranges_start = ranges_start,
#   #   ranges_width = ranges_width,
#   #   ranges_names = ranges_names,
#   #   strand = strand,
#   #   .progress = T
#   #
#   # )
#
# }

# cgr_as_lst(y)
# ?furrr::furrr_options()
# seq_len(nrow(x)) |>
#   purrr::map(slice_cgr, x = y, .progress = TRUE) -> ylist
