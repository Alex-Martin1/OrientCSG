test_that("basic vector operations work", {
  expect_equal(dot3(c(1, 0, 0), c(0, 1, 0)), 0)
  expect_equal(cross3(c(1, 0, 0), c(0, 1, 0)), c(0, 0, 1))
  expect_equal(nrm(c(2, 0, 0)), c(1, 0, 0))
})
