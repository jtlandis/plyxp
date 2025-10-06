#' @include vctrs-S4-.R

#' @title chop a vector
#' @name vctrs-vec_chop
#' @description
#' A re-export of [`vctrs::vec_chop`][vctrs::vec_chop] as an S7 generic
#' function to allow `S4Vectors`.
#' @inheritParams vctrs::vec_chop
#' @return a S3 or S4 vector
#' @examples
#' vec_chop(1L)
#' vec_chop(S4Vectors::Rle(c(rep(1, 3), rep(4, 5))), indices = list(c(2, 3, 4), c(1, 5:8)))
#'
#' @export
vec_chop <- new_generic(
  "vec_chop", "x",
  function(x, ..., indices = NULL) S7_dispatch()
)


method(
  vec_chop,
  class_vctrs
) <-
  function(x,
           ...,
           sizes = NULL,
           indices = NULL) {
    vctrs::vec_chop(x = x, ..., indices = indices, sizes = sizes)
  }



# method(
#   vec_chop,
#   class_vctrs_vec
# ) <-
#   function(x,
#            ...,
#            sizes = NULL,
#            indices = NULL) {
#     vctrs::vec_chop(x = x, ..., indices = indices, sizes = sizes)
#   }


method(
  vec_chop,
  class_s4_vctrs
) <-
  function(x,
           ...,
           indices = NULL) {
    fun <- method(vec_slice, object = x)
    if (is.null(indices)) {
      indices <- seq_len(length(x))
    }
    lapply(indices, fun, x = x)
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
