# The following variables are referred to via NSE syntax
# somewhere in plyxp. As such, they show up as NOTES in
# R CMD Check. This is to satisfy R CMD Check
name <- NULL
.features <- NULL
.samples <- NULL
.rows_group_id <- NULL
key <- loc <- NULL

## from mask_env_top.R
## these bindings are ceated in the data-mask
`plyxp:::ctx::nrow` <- NULL
`plyxp:::ctx::ncol` <- NULL
.group_id <- NULL
`.rows::.indices_group_id` <- NULL
`.cols::.indices_group_id` <- NULL
`plyxp:::ctx:::n_groups` <- NULL
`plyxp:::ctx` <- NULL
`plyxp:::ctx:::group_chop_ids` <- NULL
`plyxp:::ctx:::group_id` <- NULL
`plyxp:::ctx:::ncol` <- NULL
`plyxp:::ctx:::nrow` <- NULL
`plyxp:::assays:::group_chop_ids` <- NULL
`plyxp:::rows:::group_chop_ids` <- NULL
`plyxp:::cols:::group_chop_ids` <- NULL
`plyxp:::dim:::nrow` <- NULL
`plyxp:::dim:::ncol` <- NULL
`plyxp:::dim:::size` <- NULL
`plyxp:::dim:::n` <- NULL