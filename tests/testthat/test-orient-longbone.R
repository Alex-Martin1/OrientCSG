test_that("orient_longbone() works for TIBIA mode", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
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

  expect_true(as.numeric(res$summary$`Bio_Length_&_Orient`[1]) > 0)
  expect_equal(names(res$summary)[6], "Bio_Length_&_Orient")
  expect_equal(res$summary$`Bio_Length_&_Orient`[2], "-1\\0\\0\\0\\-1\\0")
  expect_equal(res$summary$`Bio_Length_&_Orient`[3], "0\\0\\0")
  expect_equal(res$summary$`Bio_Length_&_Orient`[4], "0\\0\\0.3")

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
    dicom_orientation = dicom_orientation_flip_xy,
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
    dicom_orientation = dicom_orientation_flip_xy,
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

  expect_true(as.numeric(res$summary$`Bio_Length_&_Orient`[1]) > 0)

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
    dicom_orientation = dicom_orientation_flip_xy,
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

  expect_true(as.numeric(res$summary$`Bio_Length_&_Orient`[1]) > 0)

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
    dicom_orientation = dicom_orientation_flip_xy,
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



test_that("current BoneJ Log eigenvector output is accepted verbatim", {
  bonej_log <- "
[INFO] ||0.018|-0.826|-0.563||
[INFO] ||-0.019|0.562|-0.827||
[INFO] ||-1.000|-0.026|0.005||
"

  expected <- matrix(
    c(
      0.018, -0.826, -0.563,
      -0.019, 0.562, -0.827,
      -1.000, -0.026, 0.005
    ),
    nrow = 3L,
    byrow = TRUE
  )

  parsed <- parse_bonej_eigenvectors(bonej_log)

  expect_equal(parsed, expected, tolerance = 1e-12)
  expect_equal(unname(parsed[, 1]), c(0.018, -0.019, -1.000), tolerance = 1e-12)

  tibia_log <- "
[INFO] ||0.008|-0.758|-0.653||
[INFO] ||0.017|-0.652|0.758||
[INFO] ||1.000|0.017|-0.008||
"

  res_log <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = tibia_log,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    individual_id = "TIBIA_LOG"
  )

  res_matrix <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    individual_id = "TIBIA_LOG"
  )

  expect_equal(res_log$bonej$eigenvectors, res_matrix$bonej$eigenvectors, tolerance = 1e-12)
  expect_equal(res_log$vectors$L, res_matrix$vectors$L, tolerance = 1e-12)
  expect_equal(res_log$section_points, res_matrix$section_points, tolerance = 1e-12)
  expect_identical(res_log$avizo_tcl, res_matrix$avizo_tcl)
})

test_that("orient_longbone() accepts BoneJ Results-table row input", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = bonej_results_row_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
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

test_that("orient_longbone() accepts a direct three-component BoneJ longitudinal vector", {
  res_vector <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = "0 0 1",
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = femur_landmarks_str,
    section_loc = 50,
    individual_id = "FEMUR_INPUT_EQUIVALENCE"
  )

  res_matrix <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = femur_landmarks_str,
    section_loc = 50,
    individual_id = "FEMUR_INPUT_EQUIVALENCE"
  )

  expect_equal(dim(res_vector$bonej$eigenvectors), c(3L, 1L))
  expect_equal(
    unname(res_vector$bonej$eigenvectors[, 1]),
    c(0, 0, 1),
    tolerance = 1e-12
  )
  expect_equal(res_vector$vectors$L, res_matrix$vectors$L, tolerance = 1e-12)
  expect_equal(res_vector$section_points, res_matrix$section_points, tolerance = 1e-12)
  expect_equal(res_vector$summary, res_matrix$summary, tolerance = 1e-12)
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
      longitudinal_matrix_str = "1 2 3 4",
      dicom_orientation = dicom_orientation_flip_xy,
      landmarks_str = tibia_landmarks_str
    ),
    "must contain either 3 numeric values"
  )

  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      dicom_orientation = dicom_orientation_flip_xy,
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
    "dicom_orientation"
  )

  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      dicom_orientation = c(dicom_iop_flip_xy, dicom_ipp_1_flip_xy),
      landmarks_str = tibia_landmarks_str
    ),
    "exactly three"
  )

  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      dicom_orientation = c(
        dicom_iop_flip_xy,
        "0020,0032 Image Position (Patient): 0\\0\\0",
        "0020,0032 Image Position (Patient): 1\\0\\0"
      ),
      landmarks_str = tibia_landmarks_str
    ),
    "not parallel"
  )

})

test_that("orient_longbone() accepts Slicer table text through landmarks_str", {
  humerus_lps <- matrix_from_xyz_string(humerus_landmarks_str)
  humerus_lps_slicer <- make_slicer_markup_table(humerus_lps)
  humerus_ras_slicer <- make_slicer_markup_table(flip_xyz_matrix(humerus_lps))

  res_plain <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = humerus_landmarks_str,
    section_loc = c(35, 50),
    lm_coord_system = "LPS"
  )
  res_lps_slicer <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = humerus_lps_slicer,
    section_loc = c(35, 50),
    lm_coord_system = "LPS"
  )
  res_slicer <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_orientation = dicom_orientation_flip_xy,
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

test_that("DICOM IOP plus ordered consecutive IPP positions control the BoneJ transform", {
  res_same_normal <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = humerus_landmarks_str,
    section_loc = 35
  )

  expect_equal(res_same_normal$bonej$coord_transform, "dicom_iop_ipp")
  expect_equal(c(res_same_normal$bonej$transform_matrix), c(diag(c(-1, -1, 1))), tolerance = 1e-12)
  expect_equal(res_same_normal$bonej$slice_direction, 1)
  expect_equal(res_same_normal$bonej$slice_spacing, 0.3, tolerance = 1e-12)
  expect_equal(res_same_normal$bonej$slice_alignment, 1, tolerance = 1e-12)

  res_carcavilla <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_carcavilla_humerus,
    dicom_orientation = dicom_orientation_carcavilla,
    landmarks_str = carcavilla_humerus_landmarks_str,
    section_loc = 35
  )

  expect_equal(c(res_carcavilla$bonej$transform_matrix), c(diag(c(1, -1, -1))), tolerance = 1e-12)
  expect_equal(res_carcavilla$bonej$slice_direction, 1)
  expect_lt(res_carcavilla$longitudinal_axis_check$angle_deg, 6)

  # Regression case matching T109 geometry: the IOP cross-product points toward
  # -Z, but stack order advances toward +Z. The ordered IPP pair must therefore
  # reverse only the slice-axis sign, yielding diag(1, -1, 1).
  t109_iop <- "0020,0037 Image Orientation (Patient): 1\\0\\0\\0\\-1\\0"
  t109_ipp_1 <- "0020,0032 Image Position (Patient): -2.04353\\43.9381\\-408.4"
  t109_ipp_2 <- "0020,0032 Image Position (Patient): -2.04353\\43.9381\\-408.1"
  t109_orientation <- c(t109_iop, t109_ipp_1, t109_ipp_2)
  t109_vector <- "0.018055801 -0.019243574 -0.999651776"
  t109_landmarks <- "
67.131508 -15.877651 -391.27063
31.569515 -10.953564 -389.399292
44.886795 -12.93088 -72.962769
"

  res_t109 <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = t109_vector,
    dicom_orientation = t109_orientation,
    landmarks_str = t109_landmarks,
    section_loc = 50,
    individual_id = "AAM_T-109_tibia_D"
  )

  expect_equal(c(res_t109$bonej$transform_matrix), c(diag(c(1, -1, 1))), tolerance = 1e-12)
  expect_equal(res_t109$bonej$slice_direction, -1)
  expect_equal(res_t109$bonej$dicom_ipp_1, c(-2.04353, 43.9381, -408.4), tolerance = 1e-12)
  expect_equal(res_t109$bonej$dicom_ipp_2, c(-2.04353, 43.9381, -408.1), tolerance = 1e-12)

  t109_orientation_block <- paste(t109_iop, t109_ipp_1, t109_ipp_2, sep = "\n")
  res_t109_block <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = t109_vector,
    dicom_orientation = t109_orientation_block,
    landmarks_str = t109_landmarks,
    section_loc = 50,
    individual_id = "AAM_T-109_tibia_D"
  )
  expect_equal(res_t109_block$bonej$transform_matrix, res_t109$bonej$transform_matrix, tolerance = 1e-12)
})

test_that("orient_longbone() supports section-only mode without anatomical planes", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
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
      dicom_orientation = dicom_orientation_flip_xy,
      landmarks_str = "150 -15 -250",
      section_loc = 35,
      USE_ANAT_ORIENT = FALSE
    )
    expect_equal(res$type, m)
    expect_equal(names(res$avizo_tcl), "SECTION_35")
  }
})

test_that("femoral Avizo/Amira camera uses anterior-up capture orientation", {
  P <- c(0, 0, 0)
  L <- c(0, 0, 1)
  ML <- c(1, 0, 0)
  AP <- c(0, -1, 0)

  camera_femur <- emit_longbone_camera(P, L, ML, AP, mode = "FEMUR")
  camera_tibia <- emit_longbone_camera(P, L, ML, AP, mode = "TIBIA")
  camera_humerus <- emit_longbone_camera(P, L, ML, AP, mode = "HUMERUS")

  expect_identical(camera_femur, camera_tibia)
  expect_false(identical(camera_femur, camera_humerus))
})

test_that("orient_longbone() works for FEMUR mode", {
  res <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_orientation = dicom_orientation_flip_xy,
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
  expect_true(as.numeric(res$summary$`Bio_Length_&_Orient`[1]) > 0)
  expect_equal(as.numeric(res$summary$`Bio_Length_&_Orient`[1]), 100, tolerance = 1e-6)

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
    dicom_orientation = dicom_orientation_flip_xy,
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
  expect_true(as.numeric(res$summary$`Bio_Length_&_Orient`[1]) > 0)
  expect_equal(as.numeric(res$summary$`Bio_Length_&_Orient`[1]), 200, tolerance = 1e-6)

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
    dicom_orientation = dicom_orientation_flip_xy,
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
  expect_contains_fixed(py_femur, "ANTERIOR_UP_SIGN = -1")

  # The solid-mesh Slicer generator uses the same femoral screen-up convention.
  res_femur_solid <- res_femur
  res_femur_solid$SOLID <- TRUE
  res_femur_solid$model_name <- "FEMUR_model"
  py_femur_solid <- emit_slicer_section_python(
    res_femur_solid,
    section = "SECTION_50"
  )
  expect_contains_fixed(py_femur_solid, "MODEL_NAME = \"FEMUR_model\"")
  expect_contains_fixed(py_femur_solid, "ANTERIOR_UP_SIGN = -1")

  res_radius <- orient_longbone(
    mode = "RADIUS",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_orientation = dicom_orientation_flip_xy,
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


test_that("TRUE-volume tibial orientation is invariant to swapping plateau landmarks", {
  xyz <- matrix_from_xyz_string(tibia_landmarks_str)
  swapped <- xyz[c(2, 1, 3), , drop = FALSE]

  res_a <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_str,
    lm_coord_system = "LPS",
    section_loc = 50,
    SLICER = TRUE,
    SOLID = FALSE
  )

  res_b <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = paste(apply(swapped, 1, paste, collapse = " "), collapse = "\n"),
    lm_coord_system = "LPS",
    section_loc = 50,
    SLICER = TRUE,
    SOLID = FALSE
  )

  expect_equal(res_a$vectors$L, res_b$vectors$L, tolerance = 1e-10)
  expect_equal(res_a$vectors$ML, res_b$vectors$ML, tolerance = 1e-10)
  expect_equal(res_a$vectors$AP, res_b$vectors$AP, tolerance = 1e-10)
  expect_equal(res_a$section_points$SECTION_50, res_b$section_points$SECTION_50, tolerance = 1e-10)

  # The canonical TRUE-volume sign must agree with the established CT
  # acquisition reference, not merely be invariant to landmark swapping.
  ct_ap_reference <- c(0, -1, 0)
  expect_gt(sum(res_a$vectors$AP * ct_ap_reference), 0)
  expect_gt(sum(res_b$vectors$AP * ct_ap_reference), 0)
})

test_that("TRUE-volume femoral orientation is invariant to swapping condylar landmarks", {
  xyz <- matrix_from_xyz_string(femur_landmarks_str)
  swapped <- xyz[c(2, 1, 3), , drop = FALSE]

  res_a <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = femur_landmarks_str,
    section_loc = 50,
    SLICER = TRUE,
    SOLID = FALSE
  )

  res_b <- orient_longbone(
    mode = "FEMUR",
    longitudinal_matrix_str = longitudinal_matrix_str_longbone_z,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = paste(apply(swapped, 1, paste, collapse = " "), collapse = "\n"),
    section_loc = 50,
    SLICER = TRUE,
    SOLID = FALSE
  )

  expect_equal(res_a$vectors$L, res_b$vectors$L, tolerance = 1e-10)
  expect_equal(res_a$vectors$ML, res_b$vectors$ML, tolerance = 1e-10)
  expect_equal(res_a$vectors$AP, res_b$vectors$AP, tolerance = 1e-10)
  expect_equal(res_a$section_points$SECTION_50, res_b$section_points$SECTION_50, tolerance = 1e-10)

  ct_ap_reference <- c(0, -1, 0)
  expect_gt(sum(res_a$vectors$AP * ct_ap_reference), 0)
  expect_gt(sum(res_b$vectors$AP * ct_ap_reference), 0)
})

test_that("tibial plain XYZ and Slicer-table inputs preserve the same landmark order", {
  xyz <- matrix_from_xyz_string(tibia_landmarks_str)
  slicer_table <- make_slicer_markup_table(xyz)

  plain <- parse_landmarks(tibia_landmarks_str, n_landmarks = 3, context = "TIBIA")
  table <- parse_landmarks(slicer_table, n_landmarks = 3, context = "TIBIA")

  expect_equal(unname(plain), unname(table), tolerance = 1e-12)
})
