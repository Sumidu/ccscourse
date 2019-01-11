context("test-likertplots")

test_that("noop", {
  data <- data.frame(a = factor(c("a","b","a")), b=factor(c("a", "b", "b")))
  plikert(data)
  warning("not implemented")
})
