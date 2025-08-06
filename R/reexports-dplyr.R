# The following functions should
# be available to users from our package

#' @title reexports from other packages
#' @name reexports
#' @returns
#' exported functions available from `plyxp`
#'
#' @seealso [plyxp::arrange()] [plyxp::mutate()] [plyxp::filter()]
#' [plyxp::summarize()] [plyxp::select()] [plyxp::pull()] [plyxp::group_by()]
#' [plyxp::group_data()] [plyxp::group_vars()] [plyxp::ungroup()]
#' @seealso [plyxp::PlySummarizedExperiment-methods]
#'
#' @examples
#' arrange(se_simple, rows(direction)) |>
#'   mutate(logp_counts = log1p(counts)) |>
#'   filter(cols(condition == "drug"))
#'
#' assays(se_simple)
#' rowData(se_simple)
#' colData(se_simple)
#'
NULL

#' @noRd
#' @export
dplyr::arrange

#' @noRd
#' @export
dplyr::mutate

#' @noRd
#' @export
dplyr::filter

#' @noRd
#' @export
dplyr::summarize

#' @noRd
#' @export
dplyr::summarise

#' @noRd
#' @export
dplyr::select

#' @noRd
#' @export
dplyr::pull

#' @noRd
#' @export
dplyr::group_by

#' @noRd
#' @export
dplyr::group_data

#' @noRd
#' @export
dplyr::group_vars

#' @noRd
#' @export
dplyr::ungroup
