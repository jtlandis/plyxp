#' @title Plyxp Simple Example Summarized Experiment
#' 
#' @description
#' A small data SummarizedExperiment Object of 20 observations, 5 rows and 4 
#' columns.
#' 
#' @format ## `se_simple`
#' \describe{
#' 
#'  \item{`assays`}{
#'    \describe{
#'      \item{counts}{sampled data points between 1:20}
#'      \item{logcounts}{log transform of `counts`}
#'    }
#'  }
#'  \item{`rowData`/`.features`}{
#'    \describe{
#'      \item{gene}{fake gene name}
#'      \item{length}{fake gene length}
#'      \item{direction}{fake strand}
#'    }
#'  }
#'  \item{`colData`/`.samples`}{
#'    \describe{
#'      \item{sample}{fake sample name}
#'      \item{condition}{control or drug treatment}
#'    }
#'  }
#' }
#' @return a `SummarizedExperiment` object
#' @examples
#' SummarizedExperiment::assays(se_simple)
#' SummarizedExperiment::rowData(se_simple)
#' SummarizedExperiment::colData(se_simple)
"se_simple"