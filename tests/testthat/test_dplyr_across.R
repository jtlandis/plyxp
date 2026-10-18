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

test_that("across erros with bad args", {
  expect_error(
    mutate(se_simple, rows(across(~ identity(.x)))),
    "You most likely meant: `across\\(everything\\(\\), ~identity\\(.x\\)\\)`"
  )

  expect_error(
    mutate(se_simple, rows(across(.fns = 1))),
    "`.fns` must be a function, a formula, or a list of functions/formulas"
  )
  expect_error(
    mutate(se_simple, rows(across(.fns = list(1)))),
    "`.fns` must be a function, a formula, or a list of functions/formulas"
  )
  expect_error(
    mutate(se_simple, rows(across(everything(), list()))),
    "Unxpected argument for `.fns`. across\\(\\) expects one of"
  )
})

test_that("across .fns can accept variable names", {
  expect_no_error(
    mutate(
      se_simple,
      across(
        everything(),
        .fns = list(
          first = \(x) x + 1,
          \(y) y + 2
        )
      )
    )
  )
  expect_no_error(
    mutate(
      se_simple,
      across(
        everything(),
        .fns = list(
          \(x) x + 1,
          \(y) y + 2
        )
      )
    )
  )
})

test_that("across paths are fine", {
  expect_no_error(
    mutate(
      se_simple,
      across(
        everything(),
        .fns = list(
          inlinable_fn = \(x) x + 1,
          uninline_fn1 = \(x, ...) x + 1,
          uninline_fn2 = \(x) return(x + 1),
          inline_fml = ~ .x + 1,
          uninline_fml1 = ~ ..1 + 1,
          uninline_fml2 = ~ {
            return(.x + 1)
          },
          dot = ~.
        )
      )
    )
  )
})
