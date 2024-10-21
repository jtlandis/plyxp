## code to prepare `se_simple` dataset goes here

set.seed(12345)
se <- SummarizedExperiment(
  list(counts = matrix(sample(1:20, 20), nrow = 5, ncol = 4)),
  rowData = data.frame(
    gene = sprintf("g%i", 1:5),
    length = rbinom(5, 100, runif(5)),
    direction = sample(c("-", "+"), 5, T)
  ),
  colData = data.frame(
    sample = sprintf("s%i", 1:4),
    condition = rep(c("cntrl", "drug"), each = 2)
  )
)
rownames(se) <- sprintf("row_%i", seq_len(5L))
colnames(se) <- sprintf("col_%i", seq_len(4L))
assay(se, "logcounts") <- log(assay(se, "counts"))
se_simple <- plyxp::new_plyxp(se)
usethis::use_data(se_simple, overwrite = TRUE)
