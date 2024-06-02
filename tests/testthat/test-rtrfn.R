test_that("rtrfn-test", {
  df<- rtrfn(500,1,3,4,6)
  x<- df$x
  expect_equal(x,df$x)
})
