% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-group_by.R
\name{group_by}
\alias{group_by}
\alias{group_by.PlySummarizedExperiment}
\alias{ungroup.PlySummarizedExperiment}
\title{apply groups to PlySummarizedExperiment}
\usage{
\method{group_by}{PlySummarizedExperiment}(.data, ..., .add = FALSE)

\method{ungroup}{PlySummarizedExperiment}(x, ...)
}
\arguments{
\item{.data}{An object Inheriting from \code{PlySummarizedExperiment}, the wrapper
class for \code{SummarizedExperiment} objects
\subsection{S4 Compatibility}{

At the moment, grouping on S4 Vectors is not yet supported. This is due to
\code{plyxp} using \verb{[vec_group_loc][vctrs::vec_group_loc]} to form grouping
information. \code{plyxp} will eventually develop a method to handle S4 Vectors.
}}

\item{...}{\link[=plyxp-context]{contextual expressions} specifying
which columns to ungroup. Omitting \code{...} ungroups the entire object.}

\item{.add}{When \code{FALSE}, the default, \code{group_by()} will override
existing groups.}

\item{x}{An object Inheriting from \code{PlySummarizedExperiment}, the wrapper
class for \code{SummarizedExperiment} objects}
}
\value{
\code{PlySummarizedExperiment} object
}
\description{
create grouping variables about the rowData and colData of a
\code{PlySummarizedExperiment} object. Unlike the \code{data.frame} method
the resulting output class is left unchanged. Thus \code{dplyr} generics for
\code{PlySummarizedExperiment} must check grouping information manually.
}
\section{Functions}{
\itemize{
\item \code{ungroup(PlySummarizedExperiment)}: Ungroup a PlySummarizedExperiment object

}}
\examples{

group_by(se_simple, rows(direction), cols(condition))

}
