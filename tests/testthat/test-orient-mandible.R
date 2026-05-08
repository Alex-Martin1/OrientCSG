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
  expect_true(res$lm9_valid)
  
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

test_that("orient_mandible() treats invalid LM9-dependent measurements as non-computable", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    lm9_valid = FALSE
  )

  expect_false(res$lm9_valid)

  corpus <- res$measurements[res$measurements$metric == "Corpus_length", ]
  expect_true(is.na(corpus$value_mm))
  expect_equal(corpus$status, "uncomputable")
  expect_equal(corpus$method, "missing_or_invalid_LM9")

  bigonial <- res$measurements[res$measurements$metric == "Bigonial_breadth", ]
  expect_true(is.na(bigonial$value_mm))
  expect_equal(bigonial$status, "uncomputable")
  expect_equal(bigonial$method, "missing_or_invalid_LM9")

  mandibular_length <- res$measurements[res$measurements$metric == "Mandibular_length", ]
  expect_false(is.na(mandibular_length$value_mm))
  expect_equal(mandibular_length$status, "direct")
})

test_that("orient_mandible() can estimate LM10 for mandibular length", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    estimate_lm10 = TRUE
  )

  expect_true(res$estimate_lm10)
  expect_named(
    res$points,
    c("LM1_Line", "LM9_Line", "LM0", "ARP_Origin", "CS1B", "CS2B", "LM10_Line"),
    ignore.order = TRUE
  )

  mandibular_length <- res$measurements[res$measurements$metric == "Mandibular_length", ]
  expect_equal(mandibular_length$status, "estimated")
  expect_contains_fixed(mandibular_length$method, "reflected_LM10_plane_P_LM2_LM3_LM4_to_LM11")
  expect_equal(
    mandibular_length$value_mm,
    round(dist3(res$points$LM10_Line, res$landmarks["LM11", ]), 6)
  )
})

test_that("orient_mandible() separates input format from coordinate system", {
  lps_mat <- matrix_from_xyz_string(mandible_landmarks_str)
  ras_slicer <- make_slicer_markup_table(flip_xyz_matrix(lps_mat))

  res_lps <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    lm_coord_system = "LPS"
  )
  res_ras <- orient_mandible(
    landmarks_str = ras_slicer,
    lm_coord_system = "RAS"
  )

  expect_equal(res_ras$lm_coord_system, "RAS")
  expect_equal(res_ras$internal_coord_system, "LPS")
  expect_equal(res_ras$landmarks, res_lps$landmarks, tolerance = 1e-6)
  expect_equal(res_ras$summary, res_lps$summary, tolerance = 1e-6)
  expect_equal(res_ras$measurements$value_mm, res_lps$measurements$value_mm, tolerance = 1e-6)
})

test_that("orient_mandible() keeps legacy coordinate and landmark aliases", {
  res_new <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    lm_coord_system = "LPS"
  )
  res_old <- orient_mandible(
    slicer_landmarks_str = mandible_landmarks_str,
    landmark_coordinate_system = "LPS"
  )

  expect_equal(res_old$landmarks, res_new$landmarks, tolerance = 1e-6)
  expect_equal(res_old$summary, res_new$summary, tolerance = 1e-6)

  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str,
      slicer_landmarks_str = mandible_landmarks_str_9
    ),
    "Both `landmarks_str` and `slicer_landmarks_str` were supplied"
  )

  expect_error(
    orient_mandible(
      landmarks_str = mandible_landmarks_str,
      lm_coord_system = "RAS",
      landmark_coordinate_system = "LPS"
    ),
    "both supplied but differ"
  )
})

test_that("orient_mandible() generates Slicer Python blocks", {
  res <- orient_mandible(
    landmarks_str = mandible_landmarks_str,
    SLICER = TRUE,
    volume_name = "MANDIBLE_VOLUME"
  )

  expect_equal(res$SLICER, TRUE)
  expect_null(res$avizo_tcl)
  expect_equal(names(res$slicer_py), c("CS1", "CS2", "CS3"))

  py_cs1 <- get_slicer_py(res, section = "CS1")
  py_cs2 <- get_slicer_py(res, section = "CS2")
  py_cs3 <- get_slicer_py(res, section = "CS3")

  expect_contains_fixed(py_cs1, "SECTION_LABEL = \"CS1\"")
  expect_contains_fixed(py_cs1, "VOLUME_NAME = \"MANDIBLE_VOLUME\"")
  expect_contains_fixed(py_cs1, "VOLUME_RENDERING_PRESET = \"CT-AAA2\"")
  expect_contains_fixed(py_cs1, "sliceNode.GetSliceToRAS().DeepCopy(sliceToRAS)")
  expect_contains_fixed(py_cs1, "THREED_VERIFICATION_VIEW_SIDE = \"PLUS\"")
  expect_contains_fixed(py_cs1, "restore_view()")
  expect_contains_fixed(py_cs1, "refresh_orientcsg_scale()")
  expect_contains_fixed(py_cs1, "PSEC, Slicer RAS")

  expect_contains_fixed(py_cs2, "SECTION_LABEL = \"CS2\"")
  expect_contains_fixed(py_cs2, "THREED_VERIFICATION_VIEW_SIDE = \"PLUS\"")
  expect_contains_fixed(py_cs3, "SECTION_LABEL = \"CS3\"")
  expect_contains_fixed(py_cs3, "THREED_VERIFICATION_VIEW_SIDE = \"MINUS\"")
  expect_contains_fixed(py_cs3, "Run restore_view()")
  expect_false(grepl("translate_orientcsg_section", py_cs3, fixed = TRUE))
})
