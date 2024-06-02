test_that("rtfn-test", {
  df<- rtfn(500,1,5)
  x<- df$x
  expect_equal(x, df$x)
})
