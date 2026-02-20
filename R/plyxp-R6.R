#' @title `plyxp` for SummarizedExperiment `assays()`
#' @name plyxp_mask-assays
#' @description
#' A more specialized version of the plyxp R6 object for the
#' assays list object. This includes chopping and unchopping
#' of matrix like objects.
#' @return an object inheriting [`plyxp`][plyxp::BiocDataMask].
#' @noRd
biocmask_assay <- R6::R6Class(
  "biocmask_assay",
  inherit = biocmask,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Create a biocmask from `.data`. `.data` is chopped by
    #' `.indices`, and environments are built from `.env`
    #'
    #' @param .data a named list like object to create a mask
    #' @param .indices the indices that will be used to chop `.data`
    #' @param .env_bot an environment that the resulting mask will be built
    #' from.
    #' @param .env_top an environment that `.env_bot` inherits from
    #' @param .nrow,.ncol the number of rows and columns of each element of
    #' `.data` respectively
    initialize = function(.data,
                          .indices,
                          .env_bot,
                          .env_top = .env_bot,
                          .nrow,
                          .ncol) {
      super$initialize(
        .data,
        .indices = .indices,
        .env_bot = .env_bot,
        .env_top = .env_top
      )
      env_bind(
        private$env_current_group_info,
        .nrow = .nrow,
        .ncol = .ncol
      )
      private$.nrow <- .nrow
      private$.ncol <- .ncol
    },
    #' @description
    #' unchop data within the mask, returns a matrix
    #' @param name name of binding to retrieve and unchop
    unchop = function(name) {
      data <- self$get_chop(name)
      if (is.null(data)) {
        return(NULL)
      }
      unchopped <- if (is.null(private$.indices)) {
        .subset2(data, 1L)
      } else {
        bioc_unchop(
          x = lapply(data, as.vector),
          ptype = as.vector(private$.ptype[[name]]),
          indices = private$.indices
        )
      }
      matrix(
        unchopped,
        nrow = private$.nrow,
        ncol = private$.ncol
      )
    }
  ),
  private = list(
    get_chop_fun = function() {
      .indices <- private$.indices
      if (is.null(.indices)) {
        # private$.e <- private$.env_col_chop <- private$env_data_chop
        return(function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        })
      } else {
        # .indices would have been created by get_group_indices()
        #
        # type <- attr(.indices, "type")
        private$.ngroups <- length(.indices)
        fun <- function(name) {
          name <- enexpr(name)
          expr(chop_assays_outer(!!name, .indices))
        }
        return(fun)
      }
    },
    .nrow = NULL,
    .ncol = NULL
  )
)

#' @export
new_biocmask.Assays <- function(.data, .indices, .top, .bot, ...) {
  biocmask_assay$new(
    .data = as(.data, "SimpleList"), .indices = .indices,
    .env_bot = .bot, .env_top = .top,
    ...
  )
}
