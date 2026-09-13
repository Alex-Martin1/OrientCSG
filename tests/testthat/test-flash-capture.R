make_flash_capture_result <- function(SLICER = FALSE, SOLID = FALSE) {
  structure(
    list(
      type = "TIBIA",
      individual_id = "T109",
      section_points = list(
        SECTION_20 = c(10, 20, 30),
        SECTION_35 = c(10, 20, 45),
        SECTION_50 = c(10, 20, 60),
        SECTION_65 = c(10, 20, 75),
        SECTION_80 = c(10, 20, 90)
      ),
      vectors = list(
        L = c(0, 0, 1),
        ML = c(1, 0, 0),
        AP = c(0, 1, 0)
      ),
      SLICER = SLICER,
      SOLID = SOLID,
      USE_ANAT_ORIENT = TRUE,
      model_name = "T109_solid"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )
}

test_that("flash_capture() generates Avizo/Amira CT batch TCL without changing the camera", {
  res <- make_flash_capture_result(SLICER = FALSE, SOLID = FALSE)

  txt <- flash_capture(
    res,
    output_dir = "C:/captures",
    sections = c(20, 50, 80),
    copy = FALSE
  )

  expect_type(txt, "character")
  expect_length(txt, 1)
  expect_contains_fixed(txt, "# Avizo/Amira / CT volume")
  expect_contains_fixed(txt, "T109_20.tif")
  expect_contains_fixed(txt, "T109_50.tif")
  expect_contains_fixed(txt, "T109_80.tif")
  expect_false(grepl("T109_35.tif", txt, fixed = TRUE))
  expect_contains_fixed(txt, '"Slice" origin setCoord')
  expect_contains_fixed(txt, "viewer 0 snapshot")
  expect_false(grepl("setCameraPosition", txt, fixed = TRUE))
  expect_false(grepl("setCameraOrientation", txt, fixed = TRUE))
})

test_that("flash_capture() generates Slicer CT Python from the prepared slice view", {
  res <- make_flash_capture_result(SLICER = TRUE, SOLID = FALSE)

  txt <- flash_capture(
    res,
    output_dir = "C:/captures",
    file_name = "T109_CT",
    sections = c(20, 80),
    slice_view = "Red",
    reference_tolerance_mm = 1.5,
    copy = FALSE
  )

  expect_contains_fixed(txt, "# 3D Slicer / CT volume")
  expect_contains_fixed(txt, "FLASH_SLICE_VIEW = 'Red'")
  expect_contains_fixed(txt, "FLASH_REFERENCE_TOLERANCE_MM = 1.500000000")
  expect_contains_fixed(txt, "FLASH_CAPTURE_SECTIONS = [")
  expect_contains_fixed(txt, "'SECTION_20',")
  expect_contains_fixed(txt, "'SECTION_80',")
  expect_contains_fixed(txt, "'SECTION_50': np.array")
  expect_contains_fixed(txt, "sliceNode.GetSliceToRAS().DeepCopy(matrix)")
  expect_contains_fixed(txt, "baselineFOV")
  expect_contains_fixed(txt, "FLASH_FILE_NAME = 'T109_CT'")
  expect_false(grepl("vtkCutter", txt, fixed = TRUE))
})

test_that("flash_capture() generates Slicer SOLID Python that re-cuts the mesh and preserves the camera", {
  res <- make_flash_capture_result(SLICER = TRUE, SOLID = TRUE)

  txt <- flash_capture(
    res,
    output_dir = "C:/captures",
    sections = c(35, 50, 65),
    reference_tolerance_mm = 2,
    copy = FALSE
  )

  expect_contains_fixed(txt, "# 3D Slicer / SOLID mesh")
  expect_contains_fixed(txt, "FLASH_MODEL_NAME = 'T109_solid'")
  expect_contains_fixed(txt, "vtk.vtkCutter()")
  expect_contains_fixed(txt, "vtk.vtkContourTriangulator()")
  expect_contains_fixed(txt, "baselinePosition = np.array(camera.GetPosition()")
  expect_contains_fixed(txt, "baselineParallelScale")
  expect_contains_fixed(txt, "baselinePosition + delta")
  expect_contains_fixed(txt, "sectionNode.SetAndObservePolyData(filled)")
  expect_contains_fixed(txt, "FLASH_FILE_NAME = 'T109'")
  expect_contains_fixed(txt, "'SECTION_35',")
  expect_contains_fixed(txt, "'SECTION_65',")
})

test_that("flash_capture() validates workflow and section arguments", {
  expect_error(
    flash_capture(list(), output_dir = "C:/captures", copy = FALSE)
  )

  res <- make_flash_capture_result(SLICER = FALSE, SOLID = FALSE)
  expect_error(
    flash_capture(res, output_dir = "C:/captures", sections = 25, copy = FALSE),
    "Unknown section"
  )

  res_slicer <- make_flash_capture_result(SLICER = TRUE, SOLID = FALSE)
  expect_error(
    flash_capture(res_slicer, output_dir = "C:/captures", extension = "png", copy = FALSE),
    "TIFF output only"
  )

  res_avizo_solid <- make_flash_capture_result(SLICER = FALSE, SOLID = TRUE)
  expect_error(
    flash_capture(res_avizo_solid, output_dir = "C:/captures", copy = FALSE),
    "implemented only for `SOLID = FALSE`"
  )
})
