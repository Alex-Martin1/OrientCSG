test_that("orient_longbone() generates TRUE-volume Slicer Python for HUMERUS mode", {
  humerus_landmarks_table <- "
1 -164.789749145508 15.670039176941 -68.205650329590 0 0 0 1 1 1 0 F-1 2 0
2 -186.393386840820 15.760459899902 -68.102157592773 0 0 0 1 1 1 0 F-2 2 0
3 -182.241800000000  6.976971000000 -59.921390000000 0 0 0 1 1 1 0 F-3 2 0
4 -182.721400000000 -8.127365000000 -345.482760000000 0 0 0 1 1 1 0 F-4 2 0
"

  res <- orient_longbone(
    mode = "HUMERUS",
    longitudinal_matrix_str = longitudinal_matrix_str_humerus,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = humerus_landmarks_table,
    lm_coord_system = "RAS",
    section_loc = c(35, 50),
    volume_name = "H108_volume",
    SLICER = TRUE,
    SOLID = FALSE
  )

  expect_equal(res$type, "HUMERUS")
  expect_equal(names(res$slicer_py), c("SECTION_35", "SECTION_50"))
  expect_null(res$avizo_tcl)
  expect_equal(nrow(res$summary), 9)

  py <- get_slicer_py(res, section = "SECTION_35")
  expect_contains_fixed(py, "VOLUME_NAME = \"H108_volume\"")
  expect_contains_fixed(py, "SECTION_LABEL = \"SECTION_35\"")
  expect_contains_fixed(py, "USE_ANATOMICAL_ORIENTATION = True")
  expect_contains_fixed(py, "find_volume_node(VOLUME_NAME)")
  expect_contains_fixed(py, "DISTAL_AXIS_POINT =")
  expect_contains_fixed(py, "PROXIMAL_AXIS_POINT =")
  expect_contains_fixed(py, "ORIENT_3D_CAMERA = True")
  expect_contains_fixed(py, "VIEW_FROM_PROXIMAL = True")
  expect_contains_fixed(py, "ANTERIOR_UP_SIGN = 1")
  expect_contains_fixed(py, "ML_RIGHT_SIGN = 1")
  expect_contains_fixed(py, "restore_3d_camera()")
  expect_contains_fixed(py, "restore_view()")
  expect_false(grepl("MODEL_NAME =", py, fixed = TRUE))
})

test_that("orient_longbone() generates TRUE-volume Slicer Python for TIBIA mode", {
  tibia_landmarks_table <- make_slicer_markup_table(matrix_from_xyz_string(tibia_landmarks_str))

  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_table,
    lm_coord_system = "LPS",
    section_loc = 50,
    volume_name = "T108_volume",
    SLICER = TRUE,
    SOLID = FALSE
  )

  distal_to_proximal_ref <- ((res$landmarks["P1", ] + res$landmarks["P2", ]) / 2) -
    res$landmarks["P3", ]

  expect_gt(dot3(res$vectors$L, distal_to_proximal_ref), 0)

  py <- get_slicer_py(res, section = "SECTION_50")
  expect_contains_fixed(py, "VOLUME_NAME = \"T108_volume\"")
  expect_contains_fixed(py, "SECTION_LABEL = \"SECTION_50\"")
  expect_contains_fixed(py, "USE_ANATOMICAL_ORIENTATION = True")
  expect_contains_fixed(py, "find_volume_node(VOLUME_NAME)")
  expect_contains_fixed(py, "PSEC =")
  expect_contains_fixed(py, "NORMAL =")
  expect_contains_fixed(py, "X_SCREEN_REFERENCE =")
  expect_contains_fixed(py, "Y_PREFERRED =")
  expect_contains_fixed(py, "ORIENT_3D_CAMERA = True")
  expect_contains_fixed(py, "VIEW_FROM_PROXIMAL = True")
  expect_contains_fixed(py, "ANTERIOR_UP_SIGN = -1")
  expect_contains_fixed(py, "ML_RIGHT_SIGN = -1")
  expect_contains_fixed(py, "CAMERA_DISTANCE = 1.000000")
  expect_contains_fixed(py, "BASE_FIELD_OF_VIEW_Y_MM = 70.0")
  expect_contains_fixed(py, "BASE_PARALLEL_SCALE_MM = 35.0")
  expect_contains_fixed(py, "fov_y = BASE_FIELD_OF_VIEW_Y_MM * CAMERA_DISTANCE")
  expect_contains_fixed(py, "fov_x = fov_y * aspect")
  expect_contains_fixed(py, "camera.SetPosition")
  expect_contains_fixed(py, "camera.SetParallelScale(BASE_PARALLEL_SCALE_MM * CAMERA_DISTANCE)")
  expect_contains_fixed(py, "restore_3d_camera()")
  expect_contains_fixed(py, "restore_view()")
  expect_false(grepl("MODEL_NAME =", py, fixed = TRUE))
})

test_that("solid-mesh Slicer Python uses restore_view as public helper", {
  res <- list(
    USE_ANAT_ORIENT = TRUE,
    type = "TIBIA",
    SOLID = TRUE,
    section_points = list(SECTION_50 = c(10, 20, 30)),
    vectors = list(
      L = c(0, 0, 1),
      ML = c(1, 0, 0),
      AP = c(0, 1, 0)
    ),
    projected = list(
      Proj_TibioTalar = c(10, 20, 0),
      Proj_Midpoint = c(10, 20, 100)
    ),
    model_name = "T108_solid",
    camera_distance = 1
  )

  py <- OrientCSG:::emit_slicer_section_python(res, section = "SECTION_50")
  expect_contains_fixed(py, "MODEL_NAME = \"T108_solid\"")
  expect_contains_fixed(py, "CAMERA_DISTANCE = 1.000000")
  expect_contains_fixed(py, "BASE_PARALLEL_SCALE_MM = 35.0")
  expect_contains_fixed(py, "camera.SetParallelScale(BASE_PARALLEL_SCALE_MM * CAMERA_DISTANCE)")
  expect_contains_fixed(py, "VIEW_FROM_PROXIMAL = True")
  expect_contains_fixed(py, "ANTERIOR_UP_SIGN = -1")
  expect_contains_fixed(py, "ML_RIGHT_SIGN = -1")
  expect_true(grepl("def restore_view(", py, fixed = TRUE))
})

test_that("Slicer long-bone screen signs are shared across TRUE and SOLID backends", {
  expect_equal(
    OrientCSG:::slicer_longbone_screen_signs("TIBIA", TRUE),
    list(anterior_up_sign = -1, ml_right_sign = -1)
  )
  expect_equal(
    OrientCSG:::slicer_longbone_screen_signs("FEMUR", TRUE),
    list(anterior_up_sign = -1, ml_right_sign = -1)
  )
  expect_equal(
    OrientCSG:::slicer_longbone_screen_signs("HUMERUS", TRUE),
    list(anterior_up_sign = 1, ml_right_sign = 1)
  )
  expect_equal(
    OrientCSG:::slicer_longbone_screen_signs("RADIUS", TRUE),
    list(anterior_up_sign = 1, ml_right_sign = 1)
  )
  expect_equal(
    OrientCSG:::slicer_longbone_screen_signs("ULNA", TRUE),
    list(anterior_up_sign = 1, ml_right_sign = 1)
  )
})

test_that("camera_distance controls orthographic framing in all long-bone backends", {
  res_avizo <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    camera_distance = 1.5
  )
  tcl <- get_tcl(res_avizo, section = "SECTION_50")
  expect_contains_fixed(tcl, "setCameraType orthographic")
  expect_contains_fixed(tcl, "setCameraHeight 150.000000")

  res_true <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_orientation = dicom_orientation_flip_xy,
    landmarks_str = tibia_landmarks_str,
    section_loc = 50,
    camera_distance = 1.5,
    SLICER = TRUE
  )
  py_true <- get_slicer_py(res_true, section = "SECTION_50")
  expect_contains_fixed(py_true, "CAMERA_DISTANCE = 1.500000")
  expect_contains_fixed(py_true, "BASE_FIELD_OF_VIEW_Y_MM = 70.0")
  expect_contains_fixed(py_true, "fov_y = BASE_FIELD_OF_VIEW_Y_MM * CAMERA_DISTANCE")
  expect_contains_fixed(py_true, "camera.SetParallelScale(BASE_PARALLEL_SCALE_MM * CAMERA_DISTANCE)")

  res_solid <- list(
    USE_ANAT_ORIENT = TRUE,
    type = "TIBIA",
    SOLID = TRUE,
    section_points = list(SECTION_50 = c(10, 20, 30)),
    vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
    projected = list(Proj_TibioTalar = c(10, 20, 0), Proj_Midpoint = c(10, 20, 100)),
    model_name = "T108_solid",
    camera_distance = 1.5
  )
  py_solid <- OrientCSG:::emit_slicer_section_python(res_solid, section = "SECTION_50")
  expect_contains_fixed(py_solid, "CAMERA_DISTANCE = 1.500000")
  expect_contains_fixed(py_solid, "BASE_PARALLEL_SCALE_MM = 35.0")
  expect_contains_fixed(py_solid, "camera.SetParallelScale(BASE_PARALLEL_SCALE_MM * CAMERA_DISTANCE)")
})

test_that("get_fragmented_roi() validates its public arguments", {
  expect_error(
    get_fragmented_roi("", limits = c(20, 80)),
    "`model_name` must be a single non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    get_fragmented_roi("W30", limits = c(80, 20)),
    "`limits` must satisfy 0 <= lower < upper <= 100.",
    fixed = TRUE
  )

  expect_error(
    get_fragmented_roi("W30", transverse_margin_mm = -1),
    "`transverse_margin_mm` must be a single non-negative finite number.",
    fixed = TRUE
  )

  expect_error(
    get_fragmented_roi("W30", create_la_line = NA),
    "`create_la_line` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("get_fragmented_roi() contains the expected Slicer ROI workflow", {
  body_text <- paste(deparse(body(get_fragmented_roi)), collapse = "\n")

  expect_contains_fixed(body_text, "LongMax")
  expect_contains_fixed(body_text, "vtkMRMLMarkupsROINode")
  expect_contains_fixed(body_text, "vtkMRMLMarkupsLineNode")
  expect_contains_fixed(body_text, "ConvexHull")
  expect_contains_fixed(body_text, "exactFarthestPair")
  expect_contains_fixed(body_text, "QInputDialog.getItem")
  expect_contains_fixed(body_text, "copy_to_clipboard(py)")
})
