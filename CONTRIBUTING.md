# Contributing to plyxp

Thank you for your interest in improving _plyxp_. Contributions of all kinds are
welcome, including bug reports, documentation fixes, and new features.

## Reporting issues and asking questions

Please use the [GitHub issue tracker](https://github.com/jtlandis/plyxp/issues)
for bug reports and feature requests.
For usage questions, please open a [discussion](https://github.com/jtlandis/plyxp/discussions/landing).
When reporting a bug, include:

- a minimal reproducible example (the [reprex](https://reprex.tidyverse.org/)
  package is helpful), ideally using a small `SummarizedExperiment`;
- the output of `sessionInfo()`;
- what you expected to happen and what happened instead.

Usage questions may also be asked on the
[Bioconductor support site](https://support.bioconductor.org/) using the
`plyxp` tag.

## Development setup

_plyxp_ follows Bioconductor's release model. New work targets the `devel`
branch, which corresponds to Bioconductor devel.

```r
# install development dependencies
install.packages(c("devtools", "BiocManager"))
BiocManager::install(version = "devel")

# from the root of your clone
devtools::install_dev_deps()
devtools::load_all()
```

## Running tests and checks

Tests use [testthat](https://testthat.r-lib.org/) and live in `tests/testthat/`.

```r
devtools::test()      # run the test suite
devtools::document()  # regenerate documentation after editing roxygen comments
devtools::check()     # full R CMD check
BiocCheck::BiocCheck(".")  # Bioconductor-specific checks
```

Continuous integration (`.github/workflows/check-bioc.yml`) runs these checks on
every push and pull request.

## Submitting changes

1. Fork the repository and create a feature branch from `devel`.
2. Make your change, adding tests for new behavior or fixed bugs.
3. Document user-facing changes with roxygen and add an entry to `NEWS`.
4. Bump the patch version in `DESCRIPTION` (e.g., `1.7.4` to `1.7.5`), as
   required by Bioconductor for changes to be propagated.
5. Ensure `devtools::test()` and `devtools::check()` pass.
6. Open a pull request against `devel` describing the motivation for the change
   and linking any related issues.

For larger changes, please open an issue first to discuss the design.

## Code of conduct

Please be respectful and constructive. This project follows the
[Bioconductor Code of Conduct](https://bioconductor.org/about/code-of-conduct/).
