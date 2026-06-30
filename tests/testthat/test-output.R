test_that("get_tcl() returns selected and combined TCL blocks", {
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_iop = dicom_iop_flip_xy,
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
    dicom_iop = dicom_iop_flip_xy,
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
    dicom_iop = dicom_iop_flip_xy,
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
  expect_contains_fixed(py, "def compute_camera_basis(L, ML, AP):")
  expect_contains_fixed(py, "camera = orient_3d_camera(volumeNode)")
  expect_contains_fixed(py, "restore_3d_camera()")
  expect_contains_fixed(py, "restore_view()")
  expect_false(grepl("restore_orientcsg_camera_state", py, fixed = TRUE))
  expect_false(grepl("MODEL_NAME =", py, fixed = TRUE))
})

test_that("orient_longbone() generates TRUE-volume Slicer Python for TIBIA mode", {
  tibia_landmarks_table <- make_slicer_markup_table(matrix_from_xyz_string(tibia_landmarks_str))
  
  res <- orient_longbone(
    mode = "TIBIA",
    longitudinal_matrix_str = longitudinal_matrix_str_tibia,
    dicom_iop = dicom_iop_flip_xy,
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
  expect_contains_fixed(py, "A proximal view places the camera on the proximal side")
  expect_contains_fixed(py, "camera.SetPosition")
  expect_contains_fixed(py, "camera.SetParallelScale(FIELD_OF_VIEW_MM / 2.0)")
  expect_contains_fixed(py, "restore_3d_camera()")
  expect_contains_fixed(py, "restore_view()")
  expect_false(grepl("restore_orientcsg_camera_state", py, fixed = TRUE))
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
    camera_distance_mm = 300
  )

  py <- OrientCSG:::emit_slicer_section_python(res, section = "SECTION_50")
  expect_contains_fixed(py, "MODEL_NAME = \"T108_solid\"")
  expect_contains_fixed(py, "def restore_view(modelNode=None):")
  expect_contains_fixed(py, "To restore this view later, run: restore_view()")
  expect_false(grepl("restore_orientcsg_camera_state", py, fixed = TRUE))
})

