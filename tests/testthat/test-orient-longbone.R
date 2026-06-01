test_that("orient_longbone() works for TIBIA mode", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_iop = dicom_iop_flip_xy,
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

test_that("orient_longbone() orients tibial L distal-to-proximal", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50
  )

  distal_to_proximal_ref <- ((res$landmarks["P1", ] + res$landmarks["P2", ]) / 2) -
    res$landmarks["P3", ]

  expect_gt(dot3(res$vectors$L, distal_to_proximal_ref), 0)
  expect_lt(res$longitudinal_axis_check$angle_deg, 90)
})

test_that("orient_longbone() works for HUMERUS mode", {
  res <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_iop = dicom_iop_flip_xy,
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
    dicom_iop = dicom_iop_flip_xy,
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
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = humerus_landmarks_str,
    section_loc = c(35, 50)
  )

  tcl_35 <- get_tcl(res, section = "SECTION_35")
  tcl_50 <- get_tcl(res, section = "SECTION_50")

  expect_contains_fixed(tcl_35, "# SECTION 35%")
  expect_contains_fixed(tcl_50, "# SECTION 50%")

  expect_contains_fixed(tcl_35, "\"Slice\" planeDefinition setValue 0")
  expect_contains_fixed(tcl_35, "\"ML\" planeDefinition setValue 0")
  expect_contains_fixed(tcl_35, "\"AP\" planeDefinition setValue 0")
  expect_false(grepl("\"ML\" planeVector", tcl_35, fixed = TRUE))
  expect_false(grepl("\"AP\" planeVector", tcl_35, fixed = TRUE))
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
      dicom_iop = dicom_iop_flip_xy,
      landmarks_str = "1 2 3"
    ),
    "requires 9 numeric values"
  )

  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      landmarks_str = tibia_landmarks_str
    ),
    "dicom_iop"
  )
})

test_that("orient_longbone() accepts Slicer table text through landmarks_str", {
  humerus_lps <- matrix_from_xyz_string(humerus_landmarks_str)
  humerus_lps_slicer <- make_slicer_markup_table(humerus_lps)
  humerus_ras_slicer <- make_slicer_markup_table(flip_xyz_matrix(humerus_lps))

  res_plain <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = humerus_landmarks_str,
    section_loc = c(35, 50),
    lm_coord_system = "LPS"
  )
  res_lps_slicer <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = humerus_lps_slicer,
    section_loc = c(35, 50),
    lm_coord_system = "LPS"
  )
  res_slicer <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = humerus_ras_slicer,
    section_loc = c(35, 50),
    lm_coord_system = "RAS"
  )

  expect_equal(res_lps_slicer$landmarks, res_plain$landmarks, tolerance = 1e-6)
  expect_equal(res_slicer$lm_coord_system, "RAS")
  expect_equal(res_slicer$internal_coord_system, "LPS")
  expect_equal(res_slicer$landmarks, res_plain$landmarks, tolerance = 1e-6)
  expect_equal(res_slicer$summary, res_plain$summary, tolerance = 1e-6)
})

test_that("DICOM Image Orientation Patient controls the BoneJ transform", {
  res_nubia <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = humerus_landmarks_str,
    section_loc = 35
  )

  res_flip_xy <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    landmarks_str = humerus_landmarks_str,
    section_loc = 35,
    bonej_coord_transform = "flip_xy"
  )

  expect_equal(res_nubia$bonej$coord_transform, "dicom_iop")
  expect_equal(c(res_nubia$bonej$transform_matrix), c(diag(c(-1, -1, 1))), tolerance = 1e-12)
  expect_equal(res_nubia$summary, res_flip_xy$summary, tolerance = 1e-6)

  res_carcavilla <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_carcavilla_humerus,
    dicom_iop = dicom_iop_carcavilla,
    landmarks_str = carcavilla_humerus_landmarks_str,
    section_loc = 35
  )

  expect_equal(c(res_carcavilla$bonej$transform_matrix), c(diag(c(1, -1, -1))), tolerance = 1e-12)
  expect_lt(res_carcavilla$longitudinal_axis_check$angle_deg, 6)
})
