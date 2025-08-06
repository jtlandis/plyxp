# `plyxp`

`plyxp` provides efficient abstractions to *SummarizedExperiment* such
that using common dplyr functions feels as natural to operating on a
*data.frame* or *tibble*.
`plyxp` makes use of a concise grammar for exploring and manipulating annotated
matrix data in the form of the *SummarizedExperiment*, scaling from simple to
complex operations spanning one or more tables of data.
We also aim for optimized implementations in `plyxp` to power some functionality
within the `tidySummarizedExperiment` package, which also offers a dplyr-like
interface to *SummarizedExperiment*. These two packages can easily be used in
parallel, by casting objects with the `new_plyxp` constructor to enable
`plyxp`-driven functionality.

`plyxp` uses
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask-programming.html)
from the `rlang` package in order to connect dplyr functions to
*SummarizedExperiment* slots in a manner that aims to be intuitive and avoiding
ambiguity in outcomes.

***Note:***  This package is still under active development. Feel free to reach
out to the package developers, see *Feedback* section below.

## installing `plyxp`

```r
# plyxp is available on BiocManager version 3.20
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
<<<<<<< HEAD
or `nrows(colData())`.
=======
or `nrows(colData())` respectively.
>>>>>>> RELEASE_3_21

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
