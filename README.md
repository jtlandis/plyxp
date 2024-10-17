# `plyxp`

`plyxp` provides efficient abstractions to the *SummarizedExperiment* such
that using common dplyr functions feels as natural to operating on a
*data.frame* or *tibble*. `plyxp` was built as an alternative to the 
`tidySummarizedExperiment` package but there may be a future in which their
conflicts are reconciled. `plyxp` uses 
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask-programming.html) 
from the `rlang` package in order to connect dplyr functions to
*SummarizedExperiment* slots in a manner that aims to be intuitive and avoiding
ambiguity in outcomes.

***Note:***  This package is still under active development as of Fall 2024.

## installing `plyxp`

```r
BiocManager::install("plyxp")
```

# data masking `SummarizedExperiment`

\
The `SummarizedExperiment` object contains three main components/"contexts" that we mask, 
the `assays()`, `rowData()`[^1] and `colData()`.

[^1]: At this moment `rowRanges()` is not supported in `plyxp` but may become
its own pronoun in the future.

![Simplified view of data masking structure. Figure made with [Biorender](https://biorender.com)](man/figures/Overview-bindings.png)

\
`plyxp` provides variables as-is to data **within their current contexts** enabling you 
to call S4 methods on S4 objects with `dplyr` verbs. If you require access to
variables _outside the context_, you may use 
pronouns made available through `plyxp` to specify where to find those 
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
or `nrows(colData())`.


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

`plyxp` was supported by a EOSS cycle 6 grant from The Wellcome Trust.


