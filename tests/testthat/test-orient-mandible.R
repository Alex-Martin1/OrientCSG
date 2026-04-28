test_that("orient_mandible() returns a valid mandibular orientation object", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "MANDIBLE_TEST",
    camera_distance_mm = 300,
    cs3_camera_side = "RIGHT"
  )
  
  expect_true(inherits(res, "orientcsg_mandible"))
  expect_true(inherits(res, "orientcsg_orientation"))
  
  expect_equal(res$type, "MANDIBLE")
  expect_equal(res$individual_id, "MANDIBLE_TEST")
  expect_equal(res$cs3_camera_side, "RIGHT")
  
  expect_equal(rownames(res$landmarks), paste0("LM", 1:11))
  expect_equal(names(res$avizo_tcl), c("CS1", "CS2", "CS3"))
  
  expect_true(is.data.frame(res$summary))
  expect_true(is.data.frame(res$measurements))
  expect_true(is.data.frame(res$manual_orientation))
  
  expect_equal(nrow(res$summary), 18)
  expect_equal(nrow(res$measurements), 5)
  expect_equal(nrow(res$manual_orientation), 12)
  
  expect_unit_vector(res$vectors$Vec_Penp)
  expect_unit_vector(res$vectors$Anterior_ref)
  
  expect_equal(
    unname(res$points$CS1B),
    unname(res$landmarks["LM5", ]),
    tolerance = 1e-6
  )
  
  expect_equal(
    unname(res$vectors$Vec_CS1),
    unname(res$landmarks["LM6", ] - res$landmarks["LM5", ]),
    tolerance = 1e-6
  )
})

test_that("orient_mandible() generates expected TCL blocks", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    individual_id = "MANDIBLE_TEST"
  )
  
  tcl_cs1 <- get_tcl(res, section = "CS1")
  tcl_cs2 <- get_tcl(res, section = "CS2")
  tcl_cs3 <- get_tcl(res, section = "CS3")
  
  expect_contains_fixed(tcl_cs1, "# MANDIBLE - CS1")
  expect_contains_fixed(tcl_cs2, "# MANDIBLE - CS2")
  expect_contains_fixed(tcl_cs3, "# MANDIBLE - CS3")
  
  expect_contains_fixed(tcl_cs1, "\"ARP\" planeDefinition setValue 1")
  expect_contains_fixed(tcl_cs1, "\"Slice\" planeDefinition setValue 2")
  expect_contains_fixed(tcl_cs3, "\"Slice\" normal setCoord 0")
  expect_contains_fixed(tcl_cs1, "viewer 0 setCameraType orthographic")
})

test_that("orient_mandible() validates malformed input", {
  expect_error(
    orient_mandible("1 2 3"),
    "requires 33 numeric values"
  )
  
  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str,
      cs3_camera_side = "POSTERIOR"
    )
  )
})

test_that("orient_mandible() accepts lowercase CS3 side values", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    cs3_camera_side = "left"
  )
  
  expect_equal(res$cs3_camera_side, "LEFT")
})