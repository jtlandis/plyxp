test_that("across same context works", {
  expect_no_error(
    mutate(
      se_simple,
      across(everything(), log)
    )
  )
  expect_no_error(
    mutate(
      se_simple,
      rows(across(everything(), ~ paste(.x, "foo")))
    )
  )
  expect_no_error(
    mutate(
      se_simple,
      cols(across(everything(), ~ paste(.x, "bar")))
    )
  )
})


test_that("across another context works", {
  expect_no_error(
    mutate(
      se_simple,
      across(rows(), identity),
      across(cols(), identity)
    )
  )
  expect_no_error(
    mutate(
      se_simple,
      rows(
        across(assays(), identity),
        across(cols(), identity)
      )
    )
  )
  expect_no_error(
    mutate(
      se_simple,
      cols(
        across(assays(), identity),
        across(rows(), identity)
      )
    )
  )
})
