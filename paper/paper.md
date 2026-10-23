---
title: "plyxp: Efficient and Tidy Manipulation of Annotated Matrix Data in R/Bioconductor"
tags:
  - R
  - Bioconductor
  - bioinformatics
  - tidyverse
  - data masking
  - SummarizedExperiment
authors:
  - name: Justin T. Landis
    orcid: 0000-0001-5501-4934
    corresponding: true
    affiliation: "1, 2"
  - name: Michael I. Love
    orcid: 0000-0001-8401-0545
    affiliation: "2, 3"
affiliations:
  - index: 1
    name: Bioinformatics and Computational Biology Program, University of North Carolina at Chapel Hill, United States
    ror: 0130frc33
  - index: 2
    name: Department of Genetics, University of North Carolina at Chapel Hill, United States
    ror: 0130frc33
  - index: 3
    name: Department of Biostatistics, University of North Carolina at Chapel Hill, United States
    ror: 0130frc33
date: 23 October 2026
bibliography: paper.bib
---

# Summary

Many biological experiments produce matrices containing measurements of features by samples.
These measurements are only interpretable alongside two additional tables of descriptive information: one describing the features and one describing the samples.
All three pieces must stay in sync whenever the data are filtered, reordered, or transformed; otherwise a measurement can silently become attached to the wrong feature or sample.
_plyxp_ is an R package that lets analysts manipulate these annotated matrix data using the user-friendly verbs of the _dplyr_ package [@dplyr], such as `mutate()` and `filter()`, while guaranteeing that measurements and descriptions remain aligned.
It introduces a small, explicit grammar for stating which of the three contexts an expression refers to and where its result should be stored.
_plyxp_ is distributed through Bioconductor and can be installed with `BiocManager::install("plyxp")`; the source code is available at <https://github.com/jtlandis/plyxp>.

# Statement of need

High-throughput omics data are commonly summarized as one or more matrices of counts or abundances across genomic features and samples or cells.
Analyses routinely require computations that link the matrix to its metadata, such as dividing RNA-seq counts by transcript length or by library size.
Data classes such as _ExpressionSet_ [@Gentleman2004] and _SummarizedExperiment_ (SE) [@Huber2015] in R/Bioconductor, and _AnnData_ [@Virshup2024] in Python, keep data and metadata consistent during subsetting, reordering, and binding.

Operating on these objects, however, requires learning domain-specific accessor functions.
Grouped computations over sets of rows or columns often require explicit loops or less familiar functional programming helpers, which can obscure the purpose of analysis code.
The _tidyomics_ project [@Hutchison2024] addresses this by bringing the chained, verb-based style of tidy data analysis [@Wickham2014], as implemented in _dplyr_ and the wider _tidyverse_ [@tidyverse], to Bioconductor classes.

_plyxp_ targets Bioconductor users whose analysis pipelines require SE objects for their compact structure but who want to adopt tidy-style abstractions that do not sacrifice efficiency.
Instead of converting the object to a long table as a naive use of _dplyr_ would require, _plyxp_ evaluates expressions directly against the matrix and metadata in place, using the data-masking facilities of _rlang_ [@rlang], and returns an object of the same class that was supplied.
_plyxp_ establishes flexible access across matrices and metadata in that the user chooses extracting data "as-is" or in a convenient "reshaped" form (see code example in Software design section).

# State of the field

Within _tidyomics_, tidy interfaces exist for genomic ranges (_plyranges_) [@Lee2019; @Lee2020], count matrices (_tidybulk_) [@Mangiola2021bulk], and Seurat objects (_tidyseurat_) [@Mangiola2021Seurat].
The closest existing tool is _tidySummarizedExperiment_, released with Bioconductor 3.12 in 2020, which presents an SE as an abstracted data frame by conceptually unwinding the matrix into a tidy long-form table.
This design is well suited to exploration and to piping data directly into _ggplot2_, but computations that combine matrix values with metadata pay the cost of that unwinding and of reconstructing the SE afterwards.
The long-form view also discards information about which table a new variable belongs to and requires rules for resolving name collisions across tables.
In Python, _AnnData_ provides the data structure, and _pandas_ [@McKinney2022] provides chained table operations, but no comparable grammar connects the two to our knowledge.

These limitations follow from the long-form abstraction itself.
We therefore built _plyxp_ as a separate package with its own class, _PlySummarizedExperiment_, which wraps any SE subclass (e.g., _SingleCellExperiment_) by composition.
Because the class is distinct, _plyxp_ and _tidySummarizedExperiment_ can be loaded together without method conflicts, and _tidySummarizedExperiment_ now calls _plyxp_ when possible (see Research impact statement).

# Software design

_plyxp_ defines three evaluation contexts for an object with $n$ rows and $p$ columns: `assays`, one or more $n \times p$ matrices; `rows`, vectors of length $n$ describing features; and `cols`, vectors of length $p$ describing samples.
Expressions are evaluated in the `assays` context by default, and the syntax functions `rows()` and `cols()` redirect evaluation to the metadata (\autoref{fig:syntax}A).
The same syntax determines where results are stored by `mutate()` and which dimension is subset by `filter()`.
Below we create a new _plyxp_ object (`xp`) from a SE where the rows and columns of the matrices represent genes and samples, respectively (setup code in the companion repository [@plyxp_examples]):

```r
xp <- new_plyxp(se)
xp |>
  mutate(
    log_counts = log10(counts + 1),
    cols(treated = dex == "trt"),
    cpbp = counts / .rows$gene_length,
    cpm = counts / .cols$mil_of_reads
  ) |>
  filter(rows(gene_biotype == "lincRNA"))
```

![Diagram of the _plyxp_ syntax with the _dplyr_ `mutate()` function, where the `assays`, `rows`, and `cols` contexts are represented by the green, purple, and yellow blocks, respectively. (A) Syntax within the `mutate()` call determines the context in which new values (blue blocks) are inserted. (B) Pronouns allow references between contexts. Pronouns first access the requested data "as-is" before reshaping it for the current context. Black bounding boxes indicate elements of a list. \label{fig:syntax}](application_note_syntax_00.png){width="100%"}

Contexts are connected through _rlang_ pronouns, objects that give an unambiguous lookup into another environment [@Wickham2019].
Each context is reachable through two pronouns (\autoref{fig:syntax}B).
The reshaped pronoun (e.g., `.rows`, `.assays`) returns data resized to fit the current context, so the row-metadata vector `gene_length` above is replicated to the size of the matrix.
This enables convenient and efficient per-element arithmetic to generate new assays, such as counts-per-basepair (`cpbp`) and counts-per-million (`cpm`) above.
The as-is pronoun (e.g., `.assays_asis`) returns the data exactly as stored, so that optimized routines can operate on its native shape.
For example, both expressions below add row means to the row metadata, but the first uses the vectorized `rowMeans()` on the matrix, whereas the second receives the matrix reshaped as a list of rows and iterates over it with _purrr_ [@purrr]:

```r
xp |>
  mutate(rows(
    ave  = rowMeans(.assays_asis$counts),
    ave2 = purrr::map_dbl(.assays$counts, mean)
  ))
```

Internally, each context is an _rlang_ data mask, a chain of environments in which object data and pronoun targets are bound lazily.
Thus, a cross-context transformation is computed only if the user's expression requests it, and at most once per data mask (\autoref{fig:masks}).
This avoids eagerly converting the whole object, eliminates name collisions between contexts, and makes the destination of every result unambiguous.
Grouping is likewise constrained by the matrix structure.
_plyxp_ only permits groupings that partition whole rows or columns, because arbitrary partitions of matrix elements are not guaranteed to form valid sub-matrices.
Enforcing this restriction for grouping has enabled _plyxp_ to implement the _dplyr_ `summarise()` function such that it will return an SE object.
Prior implementations of `summarise()` for SE objects returned a data frame because the dropped structural information and unrestricted grouping posed a challenge to consistently constructing an SE object.

![Simplified data-mask structure in _plyxp_. The `assays` and `rows` contexts are represented by the green and purple blocks, respectively. Grey rectangles represent environments and contain example variable bindings. Parent environments are linked to child environments by black lines, and pronoun bindings are linked to their target contexts by red lines. For brevity, neither full environment hierarchies nor all extended environments are shown. Entry points for each context are indicated by blue stars. Through _rlang_'s tidy evaluation engine, symbols within the data mask are resolved before the calling environment is searched. \label{fig:masks}](application_note_simple_masks_01.png){width="100%"}

_plyxp_ builds its abstractions upon Bioconductor-native operations such as `[` subsetting and incurs a small, fixed overhead.
Although _plyxp_ is not expected to outperform direct calls to the SE interface, its `group_by()`/`summarise()` operations are more expressive and often faster than equivalent hand-written code [@plyxp_examples].

# Research impact statement

_plyxp_ is a realized example of a new expressive grammar for access and manipulation of annotated matrix data and a framework for efficiently performing the operations implied by the syntax.
Its three evaluation contexts combined with the proposed sentinel syntax provide an unambiguous grammar that is faithful to the structure of the annotated matrix.
Specifically, each evaluation context is distinct such that objects within are of a similar "size" (e.g., for an annotated matrix of $n \times p$ dimensions, the rows, columns, and assays are size $n$, $p$ and $n \times p$, respectively).
Furthermore, identical symbols between these contexts will represent different objects, and the sentinel functions `rows()` and `cols()` provide a clear indication of which context is being referenced.
Cross-context operations are optimized in that any reshaping is done lazily and may be avoided entirely through the "as-is" pronoun variants, exemplifying the flexibility of _plyxp_.

We benchmarked a representative cross-context task, scaling the counts matrix by gene length, on the _airway_ RNA-seq dataset [@Himes2014].
_plyxp_ was more than an order of magnitude faster than unwinding the object to a long table and back, and within an order of magnitude of hand-written native R code.
Within _plyxp_, choosing the as-is pronoun over the reshaped one reduced the time needed to compute row means roughly tenfold, showing why both access modes are exposed.
Benchmark code and the package versions used are available in a companion repository [@plyxp_examples].

_plyxp_ has also been adopted outside our group.
As of the Bioconductor 3.22 release (October 2025), _tidySummarizedExperiment_ uses _plyxp_ internally in place of operations that previously required a long-form representation, so its users benefit without changing their code.
_plyxp_ is distributed through both the release and development branches of Bioconductor, where it passes the project's automated build and check process, and it was downloaded from more than 600 distinct IP addresses in the first month of the Bioconductor 3.23 release (May 2026).

# AI usage disclosure

Generative AI tools were not used in the development of the _plyxp_ software, its documentation (vignettes and reference manual), or the authoring of the original manuscript from which this paper is derived.
An AI assistant (Claude Opus 5.5, Anthropic) was used to restructure and condense that existing manuscript text to meet the Journal of Open Source Software's format and length requirements, and to draft the repository's contributing guide.
All resulting text was reviewed, edited, and verified by the human authors, who made all design, analysis, and interpretation decisions.

# Acknowledgements

The authors thank Rachel Sharp, Beatriz Campillo Miñano, Stuart Lee, and Stefano Mangiola for comments on an earlier draft.
This work is supported in part by an Essential Open Source Software (EOSS) grant from the Wellcome Trust [313919/Z/24/Z] and by the National Institutes of Health grant R01-HG009937.
The authors declare no competing interests.

# References
