quos <- plyxp_quos(
  foo,
  ctx2(foo),
  ctx3(foo),
  .ctx_default = "ctx1",
  .ctx_opt = c("ctx2", "ctx3")
)
foo_sym <- quote(foo)

test_that("`plyxp_quos` returns list of quosures", {
  expect_type(quos, "list")
  expect_s3_class(quos[[1]], "quosure")
  expect_s3_class(quos[[2]], "quosure")
  expect_s3_class(quos[[3]], "quosure")
})

test_that("sentinel quosures return inner expressions", {
  expect_identical(rlang::quo_get_expr(quos[[1]]), foo_sym)
  expect_identical(rlang::quo_get_expr(quos[[2]]), foo_sym)
  expect_identical(rlang::quo_get_expr(quos[[3]]), foo_sym)
})


test_that("quosures contain correct context attribute", {
  expect_attr_val <- function(object, attr, expected) {
    act <- quasi_label(rlang::enquo(object), arg = "object")
    act$attr <- attr(act$val, which = attr, exact = TRUE)
    expect(
      ok = !is.null(act$attr),
      sprintf("%s does not have attribute `%s`", act$lab, attr)
    )
    expect(
      ok = act$attr == expected,
      sprintf("%s's `%s` attribute was %s, expected %s", act$lab, attr,
              act$attr, expected)
    )
    invisible(act$val)
  }
  expect_attr_val(quos[[1]], "plyxp:::ctx", "ctx1")
  expect_attr_val(quos[[2]], "plyxp:::ctx", "ctx2")
  expect_attr_val(quos[[3]], "plyxp:::ctx", "ctx3")
})
