test_that("acut-trfn-test", {
  expect_equal(acuttrfn(1,2,3,5,c(0,0.5,1)), acuttrfn(1,2,3,5,acut.level = c(0,0.5,1)),ignore_attr=TRUE)
})
