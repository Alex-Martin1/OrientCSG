test_that("get_tcl() returns selected and combined TCL blocks", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50
  )
  
  selected <- get_tcl(res, section = "SECTION_50")
  combined <- get_tcl(res)
  
  expect_type(selected, "character")
  expect_length(selected, 1)
  
  expect_type(combined, "character")
  expect_length(combined, 1)
  
  expect_contains_fixed(selected, "# SECTION 50%")
  expect_contains_fixed(combined, "# SECTION 50%")
})

test_that("get_tcl() errors for missing or invalid TCL blocks", {
  res <- orient_mandible(landmarks_str = mandible_landmarks_str)
  
  expect_error(
    get_tcl(list()),
    "does not contain"
  )
  
  expect_error(
    get_tcl(res, section = "CS4"),
    "Available sections"
  )
})

test_that("write_tcl() writes a selected TCL block to disk", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50
  )
  
  path <- tempfile(fileext = ".tcl")
  
  returned_path <- write_tcl(
    res,
    file = path,
    section = "SECTION_50"
  )
  
  expect_equal(returned_path, path)
  expect_true(file.exists(path))
  
  written <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expected <- get_tcl(res, section = "SECTION_50")
  
  expect_equal(written, expected)
})