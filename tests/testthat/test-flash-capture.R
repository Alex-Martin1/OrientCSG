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
      camera_distance = 1,
      model_name = "T109_solid"
    ),
    class = c("orientcsg_longbone", "orientcsg_orientation")
  )
}

test_that("flash_capture() public API keeps viewer/tolerance internal", {
  args <- names(formals(flash_capture))
  expect_true("color_mode" %in% args)
  expect_false("viewer_id" %in% args)
  expect_false("reference_tolerance_mm" %in% args)
  expect_identical(eval(formals(flash_capture)$color_mode), c("rgb", "grayscale"))
})

test_that("flash_capture() keeps Avizo/Amira on native RGB snapshots", {
  res <- make_flash_capture_result(SLICER = FALSE, SOLID = FALSE)

  txt <- flash_capture(
    res,
    output_dir = "C:/captures",
    sections = c(20, 50, 80),
    copy = FALSE
  )

  expect_type(txt, "character")
  expect_length(txt, 1)
  expect_contains_fixed(txt, "T109_20.tif")
  expect_contains_fixed(txt, "T109_50.tif")
  expect_contains_fixed(txt, "T109_80.tif")
  expect_false(grepl("T109_35.tif", txt, fixed = TRUE))
  expect_contains_fixed(txt, '"Slice" origin setCoord')
  expect_contains_fixed(txt, '"ML" origin setCoord')
  expect_contains_fixed(txt, '"AP" origin setCoord')
  expect_contains_fixed(txt, "viewer 0 getCameraPosition")
  expect_contains_fixed(txt, "viewer 0 getCameraOrientation")
  expect_contains_fixed(txt, "viewer 0 setCameraPosition")
  expect_contains_fixed(txt, "viewer 0 setCameraOrientation")
  expect_contains_fixed(txt, '"Slice" origin getCoord 0')
  expect_contains_fixed(txt, '"Slice" origin setCoord 0')
  expect_contains_fixed(txt, "eval {\"Slice\" origin setCoord 0 $OrientCSG_anchorP}")
  expect_false(grepl("viewer 0 setCameraType", txt, fixed = TRUE))
  expect_false(grepl("viewer 0 setCameraHeight", txt, fixed = TRUE))
  expect_false(grepl("setCameraFocalDistance", txt, fixed = TRUE))
  expect_false(grepl("setCameraNearDistance", txt, fixed = TRUE))
  expect_false(grepl("setCameraFarDistance", txt, fixed = TRUE))
  expect_contains_fixed(txt, "viewer 0 snapshot $OrientCSG_file")
  expect_false(grepl("vtkImageLuminance", txt, fixed = TRUE))
  expect_false(grepl("orientcsg_tmp", txt, fixed = TRUE))

  camera_get <- regexpr("viewer 0 getCameraPosition", txt, fixed = TRUE)[1]
  camera_set <- regexpr("viewer 0 setCameraPosition", txt, fixed = TRUE)[1]
  camera_orientation <- regexpr("viewer 0 setCameraOrientation", txt, fixed = TRUE)[1]
  first_snapshot <- regexpr("viewer 0 snapshot $OrientCSG_file", txt, fixed = TRUE)[1]
  expect_gt(camera_get, 0)
  expect_gt(camera_set, camera_get)
  expect_gt(camera_orientation, camera_set)
  expect_gt(first_snapshot, camera_orientation)
  expect_identical(
    lengths(regmatches(txt, gregexpr("viewer 0 setCameraPosition", txt, fixed = TRUE)))[[1]],
    1L
  )
  expect_identical(
    lengths(regmatches(txt, gregexpr("viewer 0 setCameraOrientation", txt, fixed = TRUE)))[[1]],
    1L
  )
})

test_that("Avizo/Amira grayscale request warns and falls back to RGB", {
  res <- make_flash_capture_result(SLICER = FALSE, SOLID = FALSE)

  expect_warning(
    txt <- flash_capture(
      res,
      output_dir = "C:/captures",
      sections = 50,
      color_mode = "grayscale",
      copy = FALSE
    ),
    "using the native RGB snapshot"
  )

  expect_contains_fixed(txt, "viewer 0 snapshot $OrientCSG_file")
  expect_false(grepl("vtkImageLuminance", txt, fixed = TRUE))
  expect_false(grepl("orientcsg_tmp", txt, fixed = TRUE))
})

test_that("Slicer CT Flash Capture supports RGB and true grayscale output", {
  res <- make_flash_capture_result(SLICER = TRUE, SOLID = FALSE)

  txt_rgb <- flash_capture(
    res,
    output_dir = "C:/captures",
    file_name = "T109_CT",
    sections = c(20, 80),
    slice_view = "Red",
    copy = FALSE
  )

  expect_contains_fixed(txt_rgb, "FLASH_SLICE_VIEW = 'Red'")
  expect_contains_fixed(txt_rgb, "FLASH_REFERENCE_TOLERANCE_MM = 2.000000000")
  expect_contains_fixed(txt_rgb, "FLASH_COLOR_MODE = 'rgb'")
  expect_contains_fixed(txt_rgb, "FLASH_CAPTURE_SECTIONS = [")
  expect_contains_fixed(txt_rgb, "'SECTION_20',")
  expect_contains_fixed(txt_rgb, "'SECTION_80',")
  expect_contains_fixed(txt_rgb, "'SECTION_50': np.array")
  expect_contains_fixed(txt_rgb, "sliceNode.GetSliceToRAS().DeepCopy(matrix)")
  expect_contains_fixed(txt_rgb, "sliceNode.GetFieldOfView()")
  expect_contains_fixed(txt_rgb, "sliceNode.SetFieldOfView(")
  expect_contains_fixed(txt_rgb, "FLASH_FILE_NAME = 'T109_CT'")
  expect_false(grepl("vtkCutter", txt_rgb, fixed = TRUE))

  txt_gray <- flash_capture(
    res,
    output_dir = "C:/captures",
    file_name = "T109_CT",
    sections = 50,
    color_mode = "grayscale",
    copy = FALSE
  )

  expect_contains_fixed(txt_gray, "FLASH_COLOR_MODE = 'grayscale'")
  expect_contains_fixed(txt_gray, "vtk.vtkImageLuminance()")
  expect_contains_fixed(txt_gray, "writer.SetInputConnection(luminance.GetOutputPort())")
  expect_contains_fixed(txt_gray, "writer.SetCompressionToDeflate()")
})

test_that("Slicer SOLID Flash Capture supports RGB and true grayscale output", {
  res <- make_flash_capture_result(SLICER = TRUE, SOLID = TRUE)

  txt_rgb <- flash_capture(
    res,
    output_dir = "C:/captures",
    sections = c(35, 50, 65),
    copy = FALSE
  )

  expect_contains_fixed(txt_rgb, "FLASH_MODEL_NAME = 'T109_solid'")
  expect_contains_fixed(txt_rgb, "FLASH_REFERENCE_TOLERANCE_MM = 2.000000000")
  expect_contains_fixed(txt_rgb, "FLASH_COLOR_MODE = 'rgb'")
  expect_contains_fixed(txt_rgb, "vtk.vtkCutter()")
  expect_contains_fixed(txt_rgb, "vtk.vtkContourTriangulator()")
  expect_contains_fixed(txt_rgb, "camera.GetPosition()")
  expect_contains_fixed(txt_rgb, "camera.GetParallelScale()")
  expect_contains_fixed(txt_rgb, "camera.SetPosition(")
  expect_contains_fixed(txt_rgb, "camera.SetParallelScale(")
  expect_contains_fixed(txt_rgb, "sectionNode.SetAndObservePolyData(filled)")
  expect_contains_fixed(txt_rgb, "FLASH_FILE_NAME = 'T109'")
  expect_contains_fixed(txt_rgb, "'SECTION_35',")
  expect_contains_fixed(txt_rgb, "'SECTION_65',")

  txt_gray <- flash_capture(
    res,
    output_dir = "C:/captures",
    sections = 50,
    color_mode = "grayscale",
    copy = FALSE
  )

  expect_contains_fixed(txt_gray, "FLASH_COLOR_MODE = 'grayscale'")
  expect_contains_fixed(txt_gray, "vtk.vtkImageLuminance()")
  expect_contains_fixed(txt_gray, "writer.SetInputConnection(luminance.GetOutputPort())")
  expect_contains_fixed(txt_gray, "writer.SetCompressionToDeflate()")
})

test_that("flash_capture() validates workflow, section, color, and extension arguments", {
  expect_error(
    flash_capture(list(), output_dir = "C:/captures", copy = FALSE)
  )

  res <- make_flash_capture_result(SLICER = FALSE, SOLID = FALSE)
  expect_error(
    flash_capture(res, output_dir = "C:/captures", sections = 25, copy = FALSE),
    "Unknown section"
  )

  expect_error(
    flash_capture(res, output_dir = "C:/captures", color_mode = "cmyk", copy = FALSE),
    "arg"
  )

  expect_silent(
    flash_capture(res, output_dir = "C:/captures", extension = "png", copy = FALSE)
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
