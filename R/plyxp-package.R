#' @returns
#' API for using S4 classes with rlang data masks
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr group_data
#' @importFrom dplyr group_vars
#' @importFrom dplyr groups
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr reframe
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr rows_update
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom methods new
#' @importFrom pillar align
#' @importFrom pillar ctl_new_pillar
#' @importFrom pillar ctl_new_rowid_pillar
#' @importFrom pillar dim_desc
#' @importFrom pillar new_ornament
#' @importFrom pillar new_pillar
#' @importFrom pillar new_pillar_shaft
#' @importFrom pillar new_pillar_shaft_simple
#' @importFrom pillar pillar
#' @importFrom pillar pillar_component
#' @importFrom pillar pillar_shaft
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_format_footer
#' @importFrom pillar tbl_format_setup
#' @importFrom pillar tbl_sum
#' @importFrom purrr imap
#' @importFrom purrr iwalk
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr map2
#' @importFrom purrr pmap
#' @importFrom purrr reduce
#' @importFrom purrr walk
#' @importFrom rlang .env
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
#' @importFrom S7 method
#' @importFrom S7 method<-
#' @importFrom S7 new_generic
#' @importFrom S7 S7_dispatch
#' @importFrom SummarizedExperiment assay
#' @importFrom SummarizedExperiment assay<-
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assays<-
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
#' @importFrom SummarizedExperiment rowData
#' @importFrom SummarizedExperiment rowData<-
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr nest unnest chop
#' @importFrom tidyselect eval_select
#' @importFrom tidyselect starts_with
#' @importFrom utils .AtNames
#' @importFrom vctrs field
#' @importFrom vctrs list_unchop
#' @importFrom vctrs new_list_of
#' @importFrom vctrs new_rcrd
#' @importFrom vctrs obj_is_list
#' @importFrom vctrs vec_c
#' @importFrom vctrs vec_check_size
#' @importFrom vctrs vec_chop
#' @importFrom vctrs vec_group_loc
#' @importFrom vctrs vec_ptype
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom vctrs vec_recycle_common
#' @importFrom vctrs vec_rep
#' @importFrom vctrs vec_rep_each
#' @importFrom vctrs vec_restore
#' @importFrom vctrs vec_size_common
## usethis namespace: end
NULL

# rlang functions -- figure out all dependencies later
# abort %||% is_empty eval_tidy quo_set_env new_data_mask
# call2 new_quosure expr caller_env new_environment as_label splice enquos
# dots_list env_bind env_bind_lazy env_bind_active new_function is_expression
# env_parents inject as_data_pronoun quo_get_env syms
