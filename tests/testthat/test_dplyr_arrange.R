test_that("arrange works - ungrouped", {
  expect_error(
    arrange(
      se_simple, counts
    )
  )

  res <- expect_no_error(
    arrange(
      se_simple,
      rows(length),
      cols(dplyr::desc(sample))
    )
  )@se

  expect_identical(
    rowData(res)[["direction"]],
    c("-", "+", "+", "-", "+")
  )
  expect_identical(
    colData(res)[["condition"]],
    c("drug", "drug", "cntrl", "cntrl")
  )
})


test_that("arrange works - grouped", {
  gse <- group_by(se_simple, rows(direction), cols(condition))

  res <- expect_no_error(
    arrange(
      gse,
      rows(length),
      cols(dplyr::desc(sample))
    )
  )@se

  expect_identical(
    rowData(res)[["direction"]],
    c("-", "+", "+", "-", "+")
  )
  expect_identical(
    colData(res)[["condition"]],
    c("drug", "drug", "cntrl", "cntrl")
  )
})

test_that("arrange works - grouped .by_group = TRUE", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  # testing .by_group = TRUE
  res <- expect_no_error(
    arrange(
      gse,
      rows(length),
      cols(dplyr::desc(sample)),
      .by_group = TRUE
    )
  )@se



  expect_identical(
    rowData(res)[["direction"]],
    c("+", "+", "+", "-", "-")
  )
  expect_identical(
    colData(res)[["condition"]],
    c("cntrl", "cntrl", "drug", "drug")
  )
})

test_that("endomorphism", {
  res <- arrange(
    se_simple,
    rows(length),
    cols(dplyr::desc(sample))
  )
  endo <- local({
    ro <- order(rowData(se_simple@se)[["length"]], method = "radix")
    co <- order(colData(se_simple@se)[["sample"]],
      decreasing = TRUE, method = "radix"
    )
    se_simple[ro, co]
  })

  expect_identical(res, endo)

  gse <- group_by(se_simple, rows(direction), cols(condition)) |>
    arrange(rows(length),
      cols(dplyr::desc(sample)),
      .by_group = TRUE
    ) |>
    ungroup() |>
    _@se

  endo2 <- local({
    se <- se_simple@se
    ro <- order(rowData(se)[["direction"]],
      rowData(se)[["length"]],
      method = "radix"
    )
    co <- order(colData(se)[["condition"]],
      colData(se)[["sample"]],
      decreasing = c(FALSE, TRUE), method = "radix"
    )
    out <- se[ro, co]
    # group_by reorders rowData and colData based on groups
    rowData(out) <- rowData(out)[c(3, 1, 2)]
    colData(out) <- colData(out)[c(2, 1)]
    out
  })

  expect_identical(gse, endo2)
})
