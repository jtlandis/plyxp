# `plyxp` <a href="https://jtlandis.github.io/plyxp"><img src="man/figures/plyxp_hexsticker2.png" align="right" height="200" alt="plyxp website" style="float:right; height:200px;" /></a>

# Description

_plyxp_ proposes an expressive grammar for manipulating annotated matrix data, 
with syntax to access, modify, and append matrix data and tabular row and 
column metadata, including row-wise or column-wise grouped operations.
By defining multiple contexts and providing pronouns for specific recall 
and assignment within and across these contexts, _plyxp_ makes using common 
_dplyr_ functions as natural as working with a data.frame or tibble.

_plyxp_ is an implementation of this grammar for the R/Bioconductor ecosystem, 
with efficient abstractions for the _SummarizedExperiment_ class. 
Data within the _SummarizedExperiment_ are lazily bound to a series of environments, 
meaning expressions are evaluated only when the user forces their symbols. 
This gives users more freedom in how they choose to work with their data.
_plyxp_ uses 
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask-programming.html)
from the _rlang_ package to connect _dplyr_ verbs to _SummarizedExperiment_ 
slots in an intuitive and unambiguous manner.

**Note:** This package is still under active development. Feel free to reach
out to the package developers, see *Feedback* section below.

**Note:** The _tidySummarizedExperiment_ package, released with 
Bioconductor 3.12 in 2020, also provides _dplyr_-like access to 
_SummarizedExperiment_ objects within the tidyomics project, allowing 
datasets to be directly piped into _ggplot2_ plotting functions, 
for example. _plyxp_ and _tidySummarizedExperiment_ can be used in 
parallel, as users engage plyxp functions by casting their SE 
objects with `new_plyxp()`.

# Installing plyxp

```r
# plyxp is available via BiocManager
BiocManager::install("plyxp")
# To use the latest updated version please use the github
remotes::install_github("jtlandis/plyxp")
```

# Documentation

See the [Get started](https://jtlandis.github.io/plyxp/articles/plyxp.html)
link for the package vignette, and the 
[Reference])(https://jtlandis.github.io/plyxp/reference/index.html)
page for function man pages.

# Data masking SummarizedExperiment

\
The _SummarizedExperiment_ object contains three main components/"contexts" that we mask,
the `assays()`, `rowData()`[^1] and `colData()`.

[^1]: At this moment `rowRanges()` is not supported in `plyxp` but may become
its own pronoun in the future.

![Simplified view of data masking structure. Figure made with [Biorender](https://biorender.com)](man/figures/Overview-bindings.png)

\
_plyxp_ provides variables as-is to data **within their current contexts** enabling you
to call S4 methods on S4 objects with _dplyr_ verbs. If you require access to
variables _outside the context_, you may use
pronouns made available through _plyxp_ to specify where to find those
variables.

![Simplified view of reshaping pronouns. Arrows indicates to where the pronoun provides access. For each pronoun listed, there is an `_asis` variant that returns underlying data without reshaping it to fit the context. Figure made with [Biorender](https://biorender.com)](man/figures/Overview-pronouns.png)

\

The `.assays`, `.rows` and `.cols` pronouns outputs depends on the evaluating
context. Users should expect that the underlying data returned from `.rows` or
`.cols` pronouns in the _**assays context**_ is a vector, replicated to match
size of the assay context.
\
Alternatively, using a pronoun in either the `rows()` or `cols()`
contexts will likely return a list equal in length to either `nrows(rowData())`
or `nrows(colData())` respectively.

# Feedback

We would love to hear your feedback. Please post to
[Bioconductor support site](https://support.bioconductor.org)
or the
`#tidiness_in_bioc` Slack channel on community-bioc
for software usage help,
or post an
[Issue on GitHub](https://github.com/jtlandis/plyxp/issues),
for software development questions.

# Funding

_plyxp_ was supported by a EOSS cycle 6 grant from 
The Wellcome Trust, and an R01 from NHGRI.

# Note on plyxp for Bioc 3.21 or 3.20

_plyxp_ is still under active development. We have recently discovered an error in `group_by(xp, rows(foo)) |> summarize(some_assay = <expr>)` operations in which the resulting assay matrix was being collected incorrectly. This has been fixed with [this commit](https://github.com/jtlandis/plyxp/commit/ee3f3945761b62232df6f4c42d617fef614370f2) and has been pushed to `plyxp 1.4.3` on Bioconductor version 3.22. With this being said, we cannot update older version of plyxp on Bioconductor 3.21 and 3.20 - however we have cherry-picked this commit into the github branch images.

Thus if you wish to use _plyxp_ from Bioconductor 3.21 or 3.20, 
please install from github to ensure you have the latest fixes.

```r
remotes::install_github("jtlandis/plyxp@RELEASE_3_21")
```
