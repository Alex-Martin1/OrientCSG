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
  expect_equal(res$landmark_count, 11)
  expect_false(res$complete_arch)
  expect_false(res$estimate_lm10)
  expect_true(res$compute_bigonial)
  
  expect_equal(rownames(res$landmarks), paste0("LM", 1:11))
  expect_equal(names(res$avizo_tcl), c("CS1", "CS2", "CS3"))
  
  expect_true(is.data.frame(res$summary))
  expect_true(is.data.frame(res$measurements))
  expect_true(is.data.frame(res$manual_orientation))
  
  expect_equal(nrow(res$summary), 18)
  expect_equal(nrow(res$measurements), 5)
  expect_equal(nrow(res$manual_orientation), 11)
  expect_false(any(res$summary$metric %in% c("Vec_CS1", "Vec_CS2", "Vec_0_2")))
  expect_true(all(c("ARP_Origin", "Vec_CS1_Normal", "Vec_CS2_Normal") %in% res$summary$metric))
  expect_equal(names(res$measurements), c("Individual", "metric", "value_mm", "status", "method"))
  
  expect_unit_vector(res$vectors$Vec_Penp)
  expect_unit_vector(res$vectors$Vec_CS1_Normal)
  expect_unit_vector(res$vectors$Vec_CS2_Normal)
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
  
  expect_contains_fixed(tcl_cs1, "\"ARP\" planeDefinition setValue 0")
  expect_contains_fixed(tcl_cs1, "\"ARP\" origin setCoord 0")
  expect_contains_fixed(tcl_cs1, "\"ARP\" normal setCoord 0")
  expect_contains_fixed(tcl_cs1, "\"Slice\" planeDefinition setValue 0")
  expect_contains_fixed(tcl_cs1, "\"Slice\" normal setCoord 0")
  expect_false(grepl("planeVector1|planeVector2", tcl_cs1))
  expect_contains_fixed(tcl_cs3, "\"Slice\" normal setCoord 0")
  expect_contains_fixed(tcl_cs1, "viewer 0 setCameraType orthographic")
})

test_that("orient_mandible() validates malformed input", {
  expect_error(
    orient_mandible("1 2 3"),
    "requires 27, 33, or 36 numeric values"
  )
  
  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str,
      cs3_camera_side = "POSTERIOR"
    )
  )

  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str_9,
      estimate_lm10 = TRUE
    ),
    "estimate_lm10 = TRUE requires"
  )

  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str,
      complete_arch = NA
    ),
    "complete_arch must be TRUE or FALSE"
  )
})

test_that("orient_mandible() accepts lowercase CS3 side values", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    cs3_camera_side = "left"
  )
  
  expect_equal(res$cs3_camera_side, "LEFT")
})

test_that("orient_mandible() supports 12 landmarks with direct bigonial breadth", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str_12,
    individual_id = "MANDIBLE_12"
  )

  expect_equal(res$landmark_count, 12)
  expect_equal(rownames(res$landmarks), paste0("LM", 1:12))
  expect_equal(nrow(res$summary), 19)

  bigonial <- res$measurements[res$measurements$metric == "Bigonial_breadth", ]
  expect_equal(bigonial$status, "direct")
  expect_equal(bigonial$method, "LM9_LM12")
  expect_equal(
    bigonial$value_mm,
    round(dist3(res$landmarks["LM9", ], res$landmarks["LM12", ]), 6)
  )
})

test_that("orient_mandible() supports 9 landmarks with non-computable mandibular length", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str_9,
    individual_id = "MANDIBLE_9"
  )

  expect_equal(res$landmark_count, 9)
  expect_equal(rownames(res$landmarks), paste0("LM", 1:9))
  expect_equal(names(res$avizo_tcl), c("CS1", "CS2", "CS3"))

  mandibular_length <- res$measurements[res$measurements$metric == "Mandibular_length", ]
  expect_true(is.na(mandibular_length$value_mm))
  expect_equal(mandibular_length$status, "uncomputable")
  expect_equal(mandibular_length$method, "missing_LM10_LM11")
})

test_that("orient_mandible() supports complete-arch mode", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    complete_arch = TRUE
  )

  expect_true(res$complete_arch)
  expect_equal(
    unname(res$points$LM1_Line),
    unname(res$landmarks["LM4", ]),
    tolerance = 1e-6
  )
  expect_equal(res$reflection_method, "complete_arch_plane_LM1_LM4_LM2_ARP")

  dental <- res$measurements[res$measurements$metric == "Dental_arch_breadth", ]
  expect_equal(dental$status, "direct")
  expect_equal(dental$method, "LM1_LM4_A_Line")
  expect_equal(
    dental$value_mm,
    round(dist3(res$landmarks["LM1", ], res$landmarks["LM4", ]), 6)
  )
})

test_that("orient_mandible() can suppress bigonial breadth", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    compute_bigonial = FALSE
  )

  bigonial <- res$measurements[res$measurements$metric == "Bigonial_breadth", ]
  expect_true(is.na(bigonial$value_mm))
  expect_equal(bigonial$status, "uncomputable")
  expect_equal(bigonial$method, "bigonial_not_computed")
})

test_that("orient_mandible() can estimate LM10 for mandibular length", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    estimate_lm10 = TRUE
  )

  expect_true(res$estimate_lm10)
  expect_named(res$points, c("LM1_Line", "LM9_Line", "LM0", "ARP_Origin", "CS1B", "CS2B", "LM10_Line"))

  mandibular_length <- res$measurements[res$measurements$metric == "Mandibular_length", ]
  expect_equal(mandibular_length$status, "estimated")
  expect_contains_fixed(mandibular_length$method, "reflected_LM10_plane_P_LM2_LM3_LM4_to_LM11")
  expect_equal(
    mandibular_length$value_mm,
    round(dist3(res$points$LM10_Line, res$landmarks["LM11", ]), 6)
  )
})
