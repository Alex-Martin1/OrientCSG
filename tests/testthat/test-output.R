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
test_that("get_slicer_py() returns selected and combined Slicer blocks", {
  res <- list(
    slicer_py = list(
      SECTION_35 = "print('SECTION_35')",
      SECTION_50 = "print('SECTION_50')"
    )
  )

  selected <- get_slicer_py(res, section = "SECTION_50")
  combined <- get_slicer_py(res)

  expect_equal(selected, "print('SECTION_50')")
  expect_contains_fixed(combined, "print('SECTION_35')")
  expect_contains_fixed(combined, "print('SECTION_50')")

  expect_error(
    get_slicer_py(res, section = "SECTION_65"),
    "Available sections"
  )
})

test_that("orient_longbone() generates Slicer Python for HUMERUS mode", {
  humerus_slicer_landmarks_str <- "
1 -164.789749145508 15.670039176941 -68.205650329590 0 0 0 1 1 1 0 F-1 2 0
2 -186.393386840820 15.760459899902 -68.102157592773 0 0 0 1 1 1 0 F-2 2 0
3 -182.241800000000  6.976971000000 -59.921390000000 0 0 0 1 1 1 0 F-3 2 0
4 -182.721400000000 -8.127365000000 -345.482760000000 0 0 0 1 1 1 0 F-4 2 0
"

  res <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    slicer_landmarks_str = humerus_slicer_landmarks_str,
    landmark_coordinate_system = "RAS",
    section_loc = c(35, 50),
    model_name = "H108_solid",
    SLICER = TRUE
  )

  expect_equal(res$type, "HUMERUS")
  expect_equal(names(res$slicer_py), c("SECTION_35", "SECTION_50"))
  expect_null(res$avizo_tcl)
  expect_equal(nrow(res$summary), 9)

  py <- get_slicer_py(res, section = "SECTION_35")
  expect_contains_fixed(py, "MODEL_NAME = \"H108_solid\"")
  expect_contains_fixed(py, "ANTERIOR_UP_SIGN = 1")
  expect_contains_fixed(py, "L_AXIS_FULL_LENGTH = True")
  expect_contains_fixed(py, "restore_orientcsg_camera_state()")
  expect_false(grepl("press OrientCSG Home", py, fixed = TRUE))
})

test_that("SLICER output is intentionally not implemented for HUMERUS_TABLE", {
  expect_error(
    orient_longbone(
      mode = "HUMERUS_TABLE",
      longitudinal_matrix_str = longitudinal_matrix_str_humerus,
      landmarks_str = humerus_table_landmarks_str,
      SLICER = TRUE
    ),
    "HUMERUS_TABLE"
  )
})
