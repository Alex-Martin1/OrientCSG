make_test_dicom_header <- function(instance, z, series_uid = "1.2.3.4", iop = "-1\\0\\0\\0\\-1\\0") {
  data.frame(
    group = c("0020", "0020", "0020", "0020"),
    element = c("000E", "0013", "0032", "0037"),
    name = c("SeriesInstanceUID", "InstanceNumber", "ImagePositionPatient", "ImageOrientationPatient"),
    code = c("UI", "IS", "DS", "DS"),
    length = c(1, 1, 1, 1),
    value = c(series_uid, as.character(instance), paste0("10\\20\\", z), iop),
    sequence = c("", "", "", ""),
    stringsAsFactors = FALSE
  )
}

test_that("DICOM headers are ordered by InstanceNumber and converted to OrientCSG input", {
  headers <- list(
    slice_3 = make_test_dicom_header(3, 0.6),
    slice_1 = make_test_dicom_header(1, 0.0),
    slice_2 = make_test_dicom_header(2, 0.3)
  )

  info <- OrientCSG:::.dicom_orientation_from_headers(
    headers,
    files = c("slice_3.dcm", "slice_1.dcm", "slice_2.dcm")
  )

  expect_equal(info$order_method, "InstanceNumber ascending")
  expect_equal(info$n_slices, 3)
  expect_equal(info$instance_1, 1)
  expect_equal(info$instance_2, 2)
  expect_equal(info$file_1, "slice_1.dcm")
  expect_equal(info$file_2, "slice_2.dcm")
  expect_equal(info$iop, c(-1, 0, 0, 0, -1, 0))
  expect_equal(info$ipp_1, c(10, 20, 0.0))
  expect_equal(info$ipp_2, c(10, 20, 0.3))
  expect_equal(info$slice_spacing, 0.3, tolerance = 1e-12)
  expect_match(info$dicom_orientation[1], "0020,0037", fixed = TRUE)
  expect_match(info$dicom_orientation[2], "10\\20\\0", fixed = TRUE)
  expect_match(info$dicom_orientation[3], "10\\20\\0.3", fixed = TRUE)
})

test_that("automatic DICOM metadata reading refuses ambiguous series or ordering", {
  multiple_series <- list(
    a = make_test_dicom_header(1, 0.0, series_uid = "SERIES_A"),
    b = make_test_dicom_header(2, 0.3, series_uid = "SERIES_B")
  )
  expect_error(
    OrientCSG:::.dicom_orientation_from_headers(multiple_series),
    "Multiple DICOM SeriesInstanceUID"
  )

  duplicate_instance <- list(
    a = make_test_dicom_header(1, 0.0),
    b = make_test_dicom_header(1, 0.3)
  )
  expect_error(
    OrientCSG:::.dicom_orientation_from_headers(duplicate_instance),
    "unique InstanceNumber"
  )
})

test_that("orient_longbone() accepts one DICOM source at a time", {
  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      dicom_orientation = dicom_orientation_flip_xy,
      dicom_dir = tempdir(),
      landmarks_str = tibia_landmarks_str,
      section_loc = 50
    ),
    "Supply only one of `dicom_dir` or `dicom_orientation`"
  )

  expect_error(
    orient_longbone(
      mode = "TIBIA",
      longitudinal_matrix_str = longitudinal_matrix_str_tibia,
      landmarks_str = tibia_landmarks_str,
      section_loc = 50
    ),
    "Either `dicom_dir` or `dicom_orientation`"
  )
})

test_that("automatic DICOM reader remains internal", {
  expect_false("read_dicom_orientation" %in% getNamespaceExports("OrientCSG"))
  expect_true(exists(".read_dicom_orientation", envir = asNamespace("OrientCSG"), inherits = FALSE))
})
