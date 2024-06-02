test_that("acut-tfn-test", {
    expect_equal(acuttfn(1,2,3,c(0,0.5,1)), acuttfn(1,2,3,acut.level = c(0,0.5,1)),ignore_attr=TRUE)
})
