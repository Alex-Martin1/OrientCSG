test_that("orient_longbone() works for TIBIA mode", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    individual_id = "TIBIA_TEST"
  )
  
  expect_true(inherits(res, "orientcsg_longbone"))
  expect_true(inherits(res, "orientcsg_orientation"))
  
  expect_equal(res$type, "TIBIA")
  expect_equal(res$individual_id, "TIBIA_TEST")
  expect_equal(names(res$avizo_tcl), "SECTION_50")
  expect_equal(names(res$section_points), "SECTION_50")
  
  expect_true(is.data.frame(res$summary))
  expect_true(is.data.frame(res$manual_orientation))
  
  expect_equal(nrow(res$summary), 7)
  expect_equal(nrow(res$manual_orientation), 5)
  
  expect_true(res$summary$Bio_length[1] > 0)
  
  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$ML)
  expect_unit_vector(res$vectors$AP)
  
  expect_orthogonal(res$vectors$L, res$vectors$ML)
  expect_orthogonal(res$vectors$L, res$vectors$AP)
  expect_orthogonal(res$vectors$ML, res$vectors$AP)
})

test_that("orient_longbone() works for HUMERUS mode", {
  res <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    landmarks_str = humerus_landmarks_str,
    section_loc = c(35, 50),
    individual_id = "HUMERUS_TEST"
  )
  
  expect_true(inherits(res, "orientcsg_longbone"))
  expect_true(inherits(res, "orientcsg_orientation"))
  
  expect_equal(res$type, "HUMERUS")
  expect_equal(names(res$avizo_tcl), c("SECTION_35", "SECTION_50"))
  expect_equal(names(res$section_points), c("SECTION_35", "SECTION_50"))
  
  expect_equal(nrow(res$summary), 9)
  expect_equal(nrow(res$manual_orientation), 5)
  
  expect_true(res$summary$Bio_length[1] > 0)
  
  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$ML)
  expect_unit_vector(res$vectors$AP)
  
  expect_orthogonal(res$vectors$L, res$vectors$ML)
  expect_orthogonal(res$vectors$L, res$vectors$AP)
  expect_orthogonal(res$vectors$ML, res$vectors$AP)
})

test_that("orient_longbone() works for HUMERUS_TABLE mode", {
  res <- orient_longbone(
    mode = "HUMERUS_TABLE",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    landmarks_str = humerus_table_landmarks_str,
    section_loc = c(35, 50),
    individual_id = "HUMERUS_TABLE_TEST"
  )
  
  expect_true(inherits(res, "orientcsg_longbone"))
  expect_true(inherits(res, "orientcsg_orientation"))
  
  expect_equal(res$type, "HUMERUS_TABLE")
  expect_equal(names(res$avizo_tcl), c("SECTION_35", "SECTION_50"))
  expect_equal(names(res$section_points), c("SECTION_35", "SECTION_50"))
  
  expect_equal(nrow(res$summary), 7)
  expect_equal(nrow(res$manual_orientation), 5)
  
  expect_true(res$summary$Bio_length[1] > 0)
  
  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$ML)
  expect_unit_vector(res$vectors$AP)
  
  expect_orthogonal(res$vectors$L, res$vectors$ML)
  expect_orthogonal(res$vectors$L, res$vectors$AP)
  expect_orthogonal(res$vectors$ML, res$vectors$AP)
})

test_that("orient_longbone() generates expected TCL blocks", {
  res <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    landmarks_str = humerus_landmarks_str,
    section_loc = c(35, 50)
  )
  
  tcl_35 <- get_tcl(res, section = "SECTION_35")
  tcl_50 <- get_tcl(res, section = "SECTION_50")
  
  expect_contains_fixed(tcl_35, "# SECTION 35%")
  expect_contains_fixed(tcl_50, "# SECTION 50%")
  
  expect_contains_fixed(tcl_35, "\"Slice\" planeDefinition setValue 0")
  expect_contains_fixed(tcl_35, "\"ML\" planeDefinition setValue 2")
  expect_contains_fixed(tcl_35, "\"AP\" planeDefinition setValue 2")
  expect_contains_fixed(tcl_35, "viewer 0 setCameraType orthographic")
})

test_that("orient_longbone() validates malformed input", {
  expect_error(
    orient_longbone(
      mode = "FEMUR",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      landmarks_str = tibia_landmarks_str
    ),
    "mode"
  )
  
  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      landmarks_str = tibia_landmarks_str,
      section_loc = 120
    ),
    "section_loc"
  )
  
  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = "1 2 3",
      landmarks_str = tibia_landmarks_str
    ),
    "at least 9 numeric values"
  )
  
  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      landmarks_str = "1 2 3"
    ),
    "requires 9 numeric values"
  )
})