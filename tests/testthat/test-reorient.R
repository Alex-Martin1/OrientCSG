test_that("Reorient internal generator creates non-resampling Slicer CT code", {
  res <- structure(
    list(
      type = "TIBIA",
      vectors = list(L = c(0, 0, 1), ML = c(1, 0, 0), AP = c(0, 1, 0)),
      projected = list(Proj_TibioTalar = c(10, 20, 30)),
      biomechanical_length = 100,
      internal_coord_system = "LPS",
      SLICER = TRUE,
      SOLID = FALSE,
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
      SOLID = TRUE,
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
      SOLID = FALSE,
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
      SOLID = FALSE,
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
