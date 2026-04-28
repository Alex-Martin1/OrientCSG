test_that("basic vector operations work", {
  expect_equal(dot3(c(1, 0, 0), c(0, 1, 0)), 0)
  expect_equal(dot3(c(1, 2, 3), c(4, 5, 6)), 32)
  
  expect_equal(cross3(c(1, 0, 0), c(0, 1, 0)), c(0, 0, 1))
  expect_equal(cross3(c(0, 1, 0), c(1, 0, 0)), c(0, 0, -1))
  
  expect_equal(nrm(c(2, 0, 0)), c(1, 0, 0))
  expect_equal(nrm(c(0, 3, 0)), c(0, 1, 0))
  
  expect_error(
    nrm(c(0, 0, 0)),
    "near-zero norm"
  )
})

test_that("dist3() computes Euclidean distances and validates point length", {
  expect_equal(dist3(c(1, 2, 3), c(5, 5, 3)), 5)
  expect_equal(dist3(c(0, 0, 0), c(0, 0, 0)), 0)
  
  expect_error(
    dist3(c(1, 2), c(1, 2, 3)),
    "length 3"
  )
})