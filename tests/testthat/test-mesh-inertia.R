make_box_mesh_file <- function() {
  path <- tempfile(fileext = ".obj")
  writeLines(
    c(
      "v -1 -2 -5",
      "v 1 -2 -5",
      "v 1 2 -5",
      "v -1 2 -5",
      "v -1 -2 5",
      "v 1 -2 5",
      "v 1 2 5",
      "v -1 2 5",
      "f 1 4 3",
      "f 1 3 2",
      "f 5 6 7",
      "f 5 7 8",
      "f 1 2 6",
      "f 1 6 5",
      "f 4 8 7",
      "f 4 7 3",
      "f 1 5 8",
      "f 1 8 4",
      "f 2 3 7",
      "f 2 7 6"
    ),
    path
  )
  path
}

test_that("mesh inertia recovers analytic properties of a rectangular solid", {
  skip_if_not_installed("Rvcg")

  mesh_file <- make_box_mesh_file()
  on.exit(unlink(mesh_file), add = TRUE)

  res_one <- OrientCSG:::compute_mesh_inertia_axes(
    mesh_file,
    stabilize_first_axis_negative_z = TRUE,
    chunk_size = 1L
  )

  res_many <- OrientCSG:::compute_mesh_inertia_axes(
    mesh_file,
    stabilize_first_axis_negative_z = TRUE,
    chunk_size = 5L
  )

  expected_eigenvalues <- c(
    80 * (2^2 + 4^2) / 12,
    80 * (2^2 + 10^2) / 12,
    80 * (4^2 + 10^2) / 12
  )

  expect_equal(res_one$volume, 80, tolerance = 1e-8)
  expect_equal(res_one$centroid, c(0, 0, 0), tolerance = 1e-8)
  expect_equal(res_one$eigenvalues, expected_eigenvalues, tolerance = 1e-8)
  expect_equal(unname(res_one$eigenvectors[, 1]), c(0, 0, -1), tolerance = 1e-8)
  expect_equal(unname(crossprod(res_one$eigenvectors)), unname(diag(3)), tolerance = 1e-8)
  expect_gt(det(res_one$eigenvectors), 0)

  expect_equal(res_many$volume, res_one$volume, tolerance = 1e-10)
  expect_equal(res_many$centroid, res_one$centroid, tolerance = 1e-10)
  expect_equal(res_many$inertia_tensor, res_one$inertia_tensor, tolerance = 1e-10)
  expect_equal(res_many$eigenvalues, res_one$eigenvalues, tolerance = 1e-10)
})

test_that("SOLID long-bone workflow uses the mesh-derived longitudinal axis", {
  skip_if_not_installed("Rvcg")

  mesh_file <- make_box_mesh_file()
  on.exit(unlink(mesh_file), add = TRUE)

  res <- orient_longbone(
    mode = "TIBIA",
    landmarks_str = "-1 0 5\n1 0 5\n0 0 -5",
    section_loc = 50,
    individual_id = "BOX_SOLID",
    SOLID = TRUE,
    SLICER = TRUE,
    mesh_file = mesh_file,
    model_name = "BOX_MODEL"
  )

  expect_true(inherits(res, "orientcsg_longbone"))
  expect_true(res$SOLID)
  expect_true(res$SLICER)
  expect_null(res$bonej)
  expect_equal(res$mesh_axes$volume, 80, tolerance = 1e-8)
  expect_equal(res$biomechanical_length, 10, tolerance = 1e-8)
  expect_gt(dot3(res$vectors$L, c(0, 0, 1)), 0)
  expect_equal(unname(res$section_points$SECTION_50), c(0, 0, 0), tolerance = 1e-8)
  expect_equal(names(res$slicer_py), "SECTION_50")
  expect_equal(res$model_name, "BOX_MODEL")
})


test_that("SOLID ULNA workflow uses projected ulnar biomechanical length", {
  skip_if_not_installed("Rvcg")

  mesh_file <- make_box_mesh_file()
  on.exit(unlink(mesh_file), add = TRUE)

  res <- orient_longbone(
    mode = "ULNA",
    landmarks_str = "-1 0 5\n1 0 5\n0.5 1 5\n0 0 -5",
    section_loc = 50,
    individual_id = "ULNA_BOX_SOLID",
    SOLID = TRUE,
    SLICER = TRUE,
    mesh_file = mesh_file,
    model_name = "ULNA_BOX_MODEL"
  )

  expect_equal(res$biomechanical_length, 10, tolerance = 1e-8)
  expect_gt(dot3(res$vectors$L, res$landmarks["P3", ] - res$landmarks["P4", ]), 0)
  expect_gt(dot3(res$vectors$AP, res$landmarks["P3", ] - res$landmarks["P2", ]), 0)
  expect_equal(unname(res$section_points$SECTION_50), c(0, 0, 0), tolerance = 1e-8)
  expect_equal(names(res$slicer_py), "SECTION_50")
  expect_contains_fixed(res$slicer_py$SECTION_50, "MODEL_NAME = \"ULNA_BOX_MODEL\"")
})
