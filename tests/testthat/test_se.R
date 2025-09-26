test_that("Plyxp can be made from plyxp", {
  expect_identical(new_plyxp(se_simple), se_simple)
})

test_that("se() returns identity", {
  se_ <- se(se_simple)
  expect_identical(se(se_), se_)
})

test_that("`se<-`() will assign values", {
  se_ <- se(se_simple)
  rowData(se_)$foo <- 1L
  se_2 <- se(se_simple)
  se(se_2) <- se(se_)
  expect_identical(se_2, se_)
})
