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



test_that("orient_longbone() accepts BoneJ Results-table row input", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = bonej_results_row_tibia,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    individual_id = "TIBIA_RESULTS_ROW"
  )

  expect_equal(
    unname(res$bonej$eigenvectors[, 1]),
    c(
      -7.308923363019568E-4,
      0.019683244291018333,
      0.9998059990270977
    ),
    tolerance = 1e-12
  )
  expect_unit_vector(res$vectors$L)
})

test_that("orient_longbone() validates malformed input", {
  expect_error(
    orient_longbone(
      mode = "ULNA",
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

test_that("orient_longbone() supports section-only mode without anatomical planes", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = "150 -15 -250",
    section_loc = 50,
    individual_id = "TIBIA_SECTION_ONLY",
    USE_ANAT_ORIENT = FALSE
  )

  expect_false(res$USE_ANAT_ORIENT)
  expect_equal(nrow(res$landmarks), 1)
  expect_equal(nrow(res$summary), 2)
  expect_equal(nrow(res$manual_orientation), 1)
  expect_null(res$vectors$ML)
  expect_null(res$vectors$AP)
  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$X_screen)
  expect_unit_vector(res$vectors$Y_screen)
  expect_orthogonal(res$vectors$L, res$vectors$X_screen)
  expect_orthogonal(res$vectors$L, res$vectors$Y_screen)

  tcl <- get_tcl(res, section = "SECTION_50")
  expect_contains_fixed(tcl, "\"Slice\" planeDefinition setValue 0")
  expect_contains_fixed(tcl, "Section-only mode: anatomical ML/AP visual planes are not generated")
  expect_false(grepl("\"ML\" planeDefinition", tcl, fixed = TRUE))
  expect_false(grepl("\"AP\" planeDefinition", tcl, fixed = TRUE))
})

test_that("orient_longbone() section-only mode accepts all long-bone modes", {
  for (m in c("TIBIA", "HUMERUS", "FEMUR", "RADIUS", "HUMERUS_TABLE")) {
    res <- orient_longbone(
      mode = m,
      longitudinal_matrix_str = longitudinal_matrix_str_humerus,
      dicom_iop = dicom_iop_flip_xy,
      landmarks_str = "150 -15 -250",
      section_loc = 35,
      USE_ANAT_ORIENT = FALSE
    )
    expect_equal(res$type, m)
    expect_equal(names(res$avizo_tcl), "SECTION_35")
  }
})

test_that("orient_longbone() works for FEMUR mode", {
  res <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = femur_landmarks_str,
    section_loc = c(35, 50),
    individual_id = "FEMUR_TEST"
  )

  expect_true(inherits(res, "orientcsg_longbone"))
  expect_equal(res$type, "FEMUR")
  expect_equal(names(res$avizo_tcl), c("SECTION_35", "SECTION_50"))
  expect_equal(names(res$section_points), c("SECTION_35", "SECTION_50"))
  expect_equal(nrow(res$summary), 8)
  expect_equal(nrow(res$manual_orientation), 5)
  expect_true(res$summary$Bio_length[1] > 0)
  expect_equal(res$summary$Bio_length[1], 100, tolerance = 1e-6)

  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$ML)
  expect_unit_vector(res$vectors$AP)
  expect_orthogonal(res$vectors$L, res$vectors$ML)
  expect_orthogonal(res$vectors$L, res$vectors$AP)
  expect_orthogonal(res$vectors$ML, res$vectors$AP)
  expect_true(!is.null(res$projected$Proj_CondyleMidpoint))
  expect_true(!is.null(res$projected$Proj_SuperiorNeck))
})

test_that("orient_longbone() works for RADIUS mode", {
  res <- orient_longbone(
    mode = "RADIUS",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = radius_landmarks_str,
    section_loc = c(35, 50),
    individual_id = "RADIUS_TEST"
  )

  expect_true(inherits(res, "orientcsg_longbone"))
  expect_equal(res$type, "RADIUS")
  expect_equal(names(res$avizo_tcl), c("SECTION_35", "SECTION_50"))
  expect_equal(names(res$section_points), c("SECTION_35", "SECTION_50"))
  expect_equal(nrow(res$summary), 9)
  expect_equal(nrow(res$manual_orientation), 5)
  expect_true(res$summary$Bio_length[1] > 0)
  expect_equal(res$summary$Bio_length[1], 200, tolerance = 1e-6)

  expect_unit_vector(res$vectors$L)
  expect_unit_vector(res$vectors$ML)
  expect_unit_vector(res$vectors$AP)
  expect_orthogonal(res$vectors$L, res$vectors$ML)
  expect_orthogonal(res$vectors$L, res$vectors$AP)
  expect_orthogonal(res$vectors$ML, res$vectors$AP)
  expect_true(!is.null(res$projected$Proj_DistArticular))
  expect_true(!is.null(res$projected$Proj_ProxArticular))
})

test_that("orient_longbone() generates TRUE-volume Slicer Python for FEMUR and RADIUS modes", {
  res_femur <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = femur_landmarks_str,
    section_loc = 50,
    volume_name = "FEMUR_volume",
    SLICER = TRUE,
    SOLID = FALSE
  )
  py_femur <- get_slicer_py(res_femur, section = "SECTION_50")
  expect_contains_fixed(py_femur, "VOLUME_NAME = \"FEMUR_volume\"")
  expect_contains_fixed(py_femur, "USE_ANATOMICAL_ORIENTATION = True")
  expect_contains_fixed(py_femur, "DISTAL_AXIS_POINT =")
  expect_contains_fixed(py_femur, "PROXIMAL_AXIS_POINT =")
  expect_contains_fixed(py_femur, "ANTERIOR_UP_SIGN = 1")

  res_radius <- orient_longbone(
    mode = "RADIUS",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_iop = dicom_iop_flip_xy,
    landmarks_str = radius_landmarks_str,
    section_loc = 50,
    volume_name = "RADIUS_volume",
    SLICER = TRUE,
    SOLID = FALSE
  )
  py_radius <- get_slicer_py(res_radius, section = "SECTION_50")
  expect_contains_fixed(py_radius, "VOLUME_NAME = \"RADIUS_volume\"")
  expect_contains_fixed(py_radius, "USE_ANATOMICAL_ORIENTATION = True")
  expect_contains_fixed(py_radius, "DISTAL_AXIS_POINT =")
  expect_contains_fixed(py_radius, "PROXIMAL_AXIS_POINT =")
  expect_contains_fixed(py_radius, "ANTERIOR_UP_SIGN = 1")
})
