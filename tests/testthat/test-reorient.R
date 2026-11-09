test_that("Reorient internal generator creates non-resampling Slicer CT code", {
  res <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_TibioTalar = c(10, 20, 30)),
      biomechanical_length = 100,
      internal_coord_system = "LPS",
      SLICER = TRUE,
      INPUT = "VOLUME",
      USE_ANAT_ORIENT = TRUE,
      volume_name = "Tibia_CT",
      individual_id = "Tibia_CT"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  code <- OrientCSG:::.reorient_code(res)

  expect_contains_fixed(code, 'VOLUME_NAME = "Tibia_CT"')
  expect_contains_fixed(code, "ML_SIGN = -1")
  expect_contains_fixed(code, "AP_SIGN = -1")
  expect_contains_fixed(code, "vtkMRMLTransformNode")
  expect_contains_fixed(code, "redWidget.sliceLogic().SetSliceOffset(0.50 * BIO_LENGTH)")
  expect_false(grepl("Resample", code, fixed = TRUE))
})

test_that("Reorient internal generator creates a new anatomical Slicer mesh", {
  res <- structure(
    list(
      type = "ULNA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_UlnarHeadDistal = c(0, 0, 0)),
      biomechanical_length = 200,
      internal_coord_system = "LPS",
      SLICER = TRUE,
      INPUT = "MESH",
      USE_ANAT_ORIENT = TRUE,
      model_name = "W30",
      individual_id = "W30"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  code <- OrientCSG:::.reorient_code(res)

  expect_contains_fixed(code, 'MODEL_NAME = "W30"')
  expect_contains_fixed(code, "ML_SIGN = 1")
  expect_contains_fixed(code, "AP_SIGN = 1")
  expect_contains_fixed(code, "vtkTransformPolyDataFilter")
  expect_contains_fixed(code, "modelNode.GetName() + '_Anatomical'")
  expect_contains_fixed(code, "Source model was not modified.")
})

test_that("Reorient internal generator creates only the Avizo spatial transform", {
  res <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_TibioTalar = c(10, 20, 30)),
      biomechanical_length = 100,
      internal_coord_system = "LPS",
      SLICER = FALSE,
      INPUT = "VOLUME",
      USE_ANAT_ORIENT = TRUE,
      volume_name = "Tibia_CT",
      individual_id = "Tibia_CT"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  code <- OrientCSG:::.reorient_code(res)

  expect_contains_fixed(code, 'set OrientCSG_data "Tibia_CT"')
  expect_contains_fixed(code, "$OrientCSG_data setTransform")
  expect_contains_fixed(code, "10 20 -30 1")
  expect_false(grepl("applyTransform", code, fixed = TRUE))
  expect_false(grepl("create Hx", code, fixed = TRUE))
  expect_contains_fixed(code, "Resample Transformed Image manually")
})

test_that("Reorient rejects section-only and unsupported modes", {
  res <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = NULL, AP = NULL),
      projected = list(Proj_TibioTalar = c(0, 0, 0)),
      biomechanical_length = 100,
      internal_coord_system = "LPS",
      SLICER = TRUE,
      INPUT = "VOLUME",
      USE_ANAT_ORIENT = FALSE,
      volume_name = "Tibia_CT",
      individual_id = "Tibia_CT"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  expect_error(OrientCSG:::.reorient_code(res), "USE_ANAT_ORIENT = TRUE", fixed = TRUE)

  res$USE_ANAT_ORIENT <- TRUE
  res$type <- "HUMERUS_TABLE"
  res$vectors$ML <- c(1, 0, 0)
  res$vectors$AP <- c(0, 1, 0)

  expect_error(OrientCSG:::.reorient_code(res), "supports", fixed = TRUE)
})

test_that("Reorient labels AP sign limitations explicitly", {
  res <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_TibioTalar = c(0, 0, 0)),
      biomechanical_length = 100,
      internal_coord_system = "LPS",
      SLICER = TRUE,
      INPUT = "VOLUME",
      USE_ANAT_ORIENT = TRUE,
      volume_name = "Tibia_CT",
      individual_id = "Tibia_CT"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  code <- OrientCSG:::.reorient_code(res)

  expect_contains_fixed(code, "Y = AP-oriented axis; anatomically anterior only when landmark-resolved")
})


test_that("Reorient supports mandibular CT using the stored anatomical axes", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "Mandible_01",
    lm1_side = "RIGHT",
    SLICER = TRUE,
    volume_name = "Mandible_CT"
  )

  code <- OrientCSG:::.reorient_code(res)
  expect_contains_fixed(code, 'VOLUME_NAME = "Mandible_CT"')
  expect_contains_fixed(code, "ML_SIGN = 1")
  expect_contains_fixed(code, "AP_SIGN = 1")
  expect_contains_fixed(code, "redWidget.sliceLogic().SetSliceOffset(0.0)")
  expect_contains_fixed(code, "LM2 (infradentale) = (0, 0, 0)")
  expect_contains_fixed(code, "vtkMRMLTransformNode")
  expect_false(grepl("Resample", code, fixed = TRUE))

  geometry <- OrientCSG:::.reorient_mandible_geometry(res)
  expect_equal(geometry$origin, unname(as.numeric(res$landmarks["LM2", ])))
  X <- geometry$axes$vectors$ML
  Y <- geometry$axes$vectors$AP
  Z <- geometry$axes$vectors$L
  expect_equal(c(sum(X^2), sum(Y^2), sum(Z^2)), rep(1, 3), tolerance = 1e-10)
  expect_equal(c(sum(X * Y), sum(Y * Z), sum(X * Z)), rep(0, 3), tolerance = 1e-10)
  expect_gt(sum(OrientCSG:::cross3(X, Y) * Z), 0.999)
  expect_gt(sum(X * res$vectors$Vec_LeftToRight), 0.999)
  expect_gt(sum(Z * res$vectors$Vec_Penp), 0.999)
})

test_that("Reorient creates an anatomical mandibular mesh without changing input", {
  for (side in c("RIGHT", "LEFT")) {
    landmarks <- mandible_landmarks_str
    if (side == "LEFT") {
      mirrored <- OrientCSG:::parse_mandible_landmarks(landmarks)
      mirrored[, 1] <- -mirrored[, 1]
      landmarks <- paste(apply(mirrored, 1, paste, collapse = " "), collapse = "\n")
    }
    res <- orient_mandible(
      landmarks_str = landmarks,
      lm1_side = side,
      SLICER = TRUE,
      INPUT = "MESH",
      model_name = "Mandible_STL"
    )
    original_vectors <- res$vectors
    code <- OrientCSG:::.reorient_code(res)
    expect_contains_fixed(code, 'MODEL_NAME = "Mandible_STL"')
    expect_contains_fixed(code, "vtkTransformPolyDataFilter")
    expect_contains_fixed(code, "modelNode.GetName() + '_Anatomical'")
    expect_contains_fixed(code, "Source model was not modified.")
    expect_contains_fixed(code, "LM2 (infradentale) = (0, 0, 0)")
    expect_equal(res$vectors, original_vectors)
  }
})

test_that("Reorient auto-selects mandibular input and validates incompatible objects", {
  res_mesh <- orient_mandible(
    landmarks_str = mandible_landmarks_str, SLICER = TRUE,
    INPUT = "MESH", model_name = "Mandible_STL"
  )
  code <- OrientCSG:::.reorient_code(res_mesh)
  expect_contains_fixed(code, 'MODEL_NAME = "Mandible_STL"')
  expect_contains_fixed(code, "STRICT_NAME = True")

  invalid <- res_mesh
  invalid$SLICER <- FALSE
  expect_error(
    OrientCSG:::.reorient_code(invalid),
    'requires 3D Slicer', fixed = TRUE
  )

  invalid <- res_mesh
  invalid$INPUT <- "BAD"
  expect_error(OrientCSG:::.reorient_code(invalid), 'res$INPUT', fixed = TRUE)

  legacy <- orient_mandible(
    landmarks_str = mandible_landmarks_str, SLICER = TRUE,
    volume_name = "Mandible_CT"
  )
  legacy$INPUT <- NULL
  expect_contains_fixed(
    OrientCSG:::.reorient_code(legacy), 'VOLUME_NAME = "Mandible_CT"'
  )
})


test_that("Mandible Reorient auto-selects Avizo TCL for CT", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "Mandible_01",
    lm1_side = "RIGHT",
    SLICER = FALSE,
    volume_name = "Mandible_CT"
  )
  code <- OrientCSG:::.reorient_code(res)
  expect_contains_fixed(code, 'set OrientCSG_data "Mandible_CT"')
  expect_contains_fixed(code, "$OrientCSG_data setTransform")
  expect_contains_fixed(code, "X = anatomical left -> right")
  expect_contains_fixed(code, "Y = anatomical posterior -> anterior")
  expect_contains_fixed(code, "Z = anatomical inferior -> superior")
  expect_contains_fixed(code, "LM2 (infradentale) = (0, 0, 0)")
  expect_false(grepl("vtkMRMLTransformNode", code, fixed = TRUE))
  expect_false(grepl("vtkTransformPolyDataFilter", code, fixed = TRUE))
  expect_false(grepl("resample(", code, fixed = TRUE))

  geom <- OrientCSG:::.reorient_mandible_geometry(res)
  basis <- OrientCSG:::.reorient_basis_lps(
    geom$axes, list(ml_right_sign = 1, anterior_up_sign = 1)
  )
  expect_gt(sum(basis$X * res$vectors$Vec_LeftToRight), 0.999)
  expect_gt(sum(basis$Z * res$vectors$Vec_Penp), 0.999)

  # Switching SLICER changes only the output language, not the geometry.
  res$SLICER <- TRUE
  code_slicer <- OrientCSG:::.reorient_code(res)
  expect_contains_fixed(code_slicer, "vtkMRMLTransformNode")
  expect_false(grepl("$OrientCSG_data setTransform", code_slicer, fixed = TRUE))
})


test_that("Reorient object_name overrides stored names in every supported workflow", {
  longbone <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_TibioTalar = c(0, 0, 0)),
      biomechanical_length = 100,
      internal_coord_system = "LPS", SLICER = TRUE,
      INPUT = "VOLUME", USE_ANAT_ORIENT = TRUE,
      volume_name = "Old_CT", model_name = "Old_Mesh",
      individual_id = "Specimen"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )

  ct <- OrientCSG:::.reorient_code(longbone, object_name = "Named_CT")
  expect_contains_fixed(ct, 'VOLUME_NAME = "Named_CT"')
  expect_contains_fixed(ct, "STRICT_NAME = True")
  expect_contains_fixed(ct, "elif STRICT_NAME:")
  expect_contains_fixed(ct, "No volume named")

  ct_default <- OrientCSG:::.reorient_code(longbone)
  expect_contains_fixed(ct_default, 'VOLUME_NAME = "Old_CT"')
  expect_contains_fixed(ct_default, "STRICT_NAME = False")

  longbone$INPUT <- "MESH"
  mesh <- OrientCSG:::.reorient_code(longbone, object_name = "Named_Mesh")
  expect_contains_fixed(mesh, 'MODEL_NAME = "Named_Mesh"')
  expect_contains_fixed(mesh, "STRICT_NAME = True")
  expect_contains_fixed(mesh, "No model named")

  mesh_default <- OrientCSG:::.reorient_code(longbone)
  expect_contains_fixed(mesh_default, 'MODEL_NAME = "Old_Mesh"')
  expect_contains_fixed(mesh_default, "STRICT_NAME = False")

  longbone$INPUT <- "VOLUME"
  longbone$SLICER <- FALSE
  avizo <- OrientCSG:::.reorient_code(longbone, object_name = "Named_Avizo_CT")
  expect_contains_fixed(avizo, 'set OrientCSG_data "Named_Avizo_CT"')
  expect_contains_fixed(
    OrientCSG:::.reorient_code(longbone),
    'set OrientCSG_data "Old_CT"'
  )

  mandible <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "Mandible_01", lm1_side = "RIGHT",
    SLICER = TRUE, volume_name = "Mandible_CT"
  )
  mct <- OrientCSG:::.reorient_code(mandible, object_name = "Chosen_CT")
  expect_contains_fixed(mct, 'VOLUME_NAME = "Chosen_CT"')
  expect_contains_fixed(mct, "STRICT_NAME = True")
  expect_contains_fixed(OrientCSG:::.reorient_code(mandible), "STRICT_NAME = False")

  mesh <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "Mandible_01", lm1_side = "RIGHT",
    SLICER = TRUE, INPUT = "MESH", model_name = "Mandible_STL"
  )
  mmesh <- OrientCSG:::.reorient_code(mesh, object_name = "Chosen_STL")
  expect_contains_fixed(mmesh, 'MODEL_NAME = "Chosen_STL"')
  expect_contains_fixed(mmesh, "STRICT_NAME = True")
  expect_contains_fixed(mmesh, "No model named")

  mmesh_default <- OrientCSG:::.reorient_code(mesh)
  expect_contains_fixed(mmesh_default, 'MODEL_NAME = "Mandible_STL"')
  expect_contains_fixed(mmesh_default, "STRICT_NAME = True")

  mandible$SLICER <- FALSE
  expect_contains_fixed(
    OrientCSG:::.reorient_code(mandible, object_name = "Chosen_Avizo_CT"),
    'set OrientCSG_data "Chosen_Avizo_CT"'
  )
  expect_contains_fixed(
    OrientCSG:::.reorient_code(mandible),
    'set OrientCSG_data "Mandible_CT"'
  )
})

test_that("Reorient rejects invalid explicit object_name", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str, SLICER = TRUE
  )
  for (name in list("", "   ", NA_character_, 123, c("A", "B"))) {
    expect_error(
      OrientCSG:::.reorient_code(res, object_name = name),
      "single non-empty character", fixed = TRUE
    )
  }
})
