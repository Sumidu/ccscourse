context("test-hello")

test_that("hello world has not sideeffect", {
  hello()
  expect_equal(2 * 2, 4)
})
