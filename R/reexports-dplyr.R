
# The following functions should
# be available to users from our package

#' @name reexports
#' @returns 
#' exported functions available from `plyxp`
#' @examples
#' 
#' arrange(se_simple, rows(direction)) |>
#'   mutate(logp_counts = logp(counts)) |>
#'   filter(cols(condition=="drug"))
#' 
NULL

#' @export
dplyr::arrange

#' @export
dplyr::mutate

#' @export
dplyr::filter

#' @export
dplyr::summarise

#' @export
dplyr::select

#' @export
dplyr::pull

#' @export
dplyr::group_by

#' @export
dplyr::group_data

#' @export
dplyr::group_vars

#' @export
dplyr::ungroup
