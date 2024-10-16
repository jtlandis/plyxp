
#' @title New Biocmask
#' @name new_plyxp_manager
#' @description
#' Create a plyxp for an object
#' @param obj Dispatch Object
#' @param ... Not used
#' @return a plyxp_manager R6 class object
#' @seealso [plyxp::PlyxpMaskManager]
#' @examples
#'  
#' manager <- new_plyxp_manager(se_simple)
#' manager$ctx
#' q <- plyxp_quos(counts_1 = counts + 1,
#'                    cols(is_drug = condition=="drug"),
#'                    .ctx_default = "assays",
#'                    .ctx_opt = c("rows", "cols"))
#' manager$eval(q[[1]])
#' manager$results()
#' #evaluating second quo without switching contexts will error
#' manager$eval(q[[2]]) |> try()
#' manager$ctx <- "cols"
#' manager$ctx
#' manager$eval(q[[2]])
#' manager$results()
#' 
#' @export
new_plyxp_manager <- function(obj, ...) {
  UseMethod("new_plyxp_manager")
}

#' @rdname new_plyxp_manager
#' @export
new_plyxp_manager.SummarizedExperiment <- function(obj, ...) {
  groups <- group_details(obj)
  expanded <- expand_groups2(groups$row_groups, groups$col_groups)
  nr <- nrow(obj)
  nc <- ncol(obj)
  shared_ctx_env <- prepare_shared_ctx_env(groups = groups, expanded = expanded)
  
  mask_assay <- plyxp_assay$new(assays(obj),
                                   get_group_indices(groups, expanded, "assay"),
                                   .nrow = nr,
                                   .ncol = nc,
                                   .env_bot = shared_ctx_env,
                                   .env_top = top_env)
  mask_rows <- plyxp$new(prepend_rownames(rowData(obj), column = ".features"),
                            get_group_indices(groups, expanded, "rowData"),
                            .env_bot = shared_ctx_env,
                            .env_top = top_env)
  mask_cols <- plyxp$new(prepend_rownames(colData(obj), column = ".samples"),
                            get_group_indices(groups, expanded, "colData"),
                            .env_bot = shared_ctx_env,
                            .env_top = top_env)
  
  extended_environments <- connect_masks(mask_assays = mask_assay,
                                         mask_rows = mask_rows,
                                         mask_cols = mask_cols)
  
  plyxp_manager$new(.data = obj,
                       .masks = list(assays = mask_assay,
                                     rows = mask_rows,
                                     cols = mask_cols),
                       .ctx_env = shared_ctx_env,
                       .extended_env = extended_environments)
}

plyxp_evaluate <- function(mask, quos, ctxs, nams, env, .matrix = FALSE) {
  .call <- caller_call()
  if (.matrix) {
    quos <- enforce_matrix(quos, ctxs)
  }
  n_quo <- length(quos)
  try_fetch(
    {
      for(i in seq_len(n_quo)) {
        quo <- quos[[i]]
        #nm <- nams[i]
        mask$ctx <- ctxs[[i]]
        mask$eval(quo, env = env)
      }
    },
    error = function(cnd) {
      current_ctx <- mask$ctx
      current_gid <- mask$group_id
      cli::cli_abort(
        message = "an error occured in group {current_gid} of `{current_ctx}` context",
        parent = cnd,
        call = .call,
        class = "plyxp_dplyr_eval_error"
      )
    }
  )
  invisible(mask)
}
