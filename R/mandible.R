#' Orient mandibular cross-sections
#'
#' @param landmarks_str Character string containing 11 mandibular landmarks in order LM1...LM11.
#' @param individual_id Specimen identifier.
#' @param camera_distance_mm Approximate camera distance in Avizo units/mm.
#' @param cs3_camera_side Camera side for CS3: "RIGHT" or "LEFT".
#' @return A `mandori_orientation` object.
#' @export
orient_mandible <- function(landmarks_str,
                            individual_id = "MANDIBLE_001",
                            camera_distance_mm = 300,
                            cs3_camera_side = c("RIGHT", "LEFT")) {
  cs3_camera_side <- match.arg(toupper(cs3_camera_side), c("RIGHT", "LEFT"))
  mat_pts <- parse_landmarks(landmarks_str, n_landmarks = 11, context = "MANDIBLE")
  rownames(mat_pts) <- paste0("LM", 1:11)

  LM1  <- mat_pts[1, ];  LM2  <- mat_pts[2, ];  LM3  <- mat_pts[3, ];  LM4  <- mat_pts[4, ]
  LM5  <- mat_pts[5, ];  LM6  <- mat_pts[6, ];  LM7  <- mat_pts[7, ];  LM8  <- mat_pts[8, ]
  LM9  <- mat_pts[9, ];  LM10 <- mat_pts[10, ]; LM11 <- mat_pts[11, ]

  LM1_Line <- reflect_point_across_plane(LM1, LM2, LM3, LM4)
  LM9_Line <- reflect_point_across_plane(LM9, LM2, LM3, LM4)

  Vec_1_1Line <- LM1_Line - LM1
  if (sqrt(sum(Vec_1_1Line^2)) < 1e-12) stop("LM1 and LM1_Line coincide; Vec_1_1Line cannot be defined.", call. = FALSE)

  LM0 <- project_point_to_line(LM2, LM1, LM1_Line)
  Vec_0_2 <- LM2 - LM0
  if (sqrt(sum(Vec_0_2^2)) < 1e-12) stop("LM2 projects almost onto itself on the LM1--LM1_Line axis; Vec_0_2 cannot be defined.", call. = FALSE)

  Vec_Penp <- nrm(cross3(Vec_0_2, Vec_1_1Line))
  CS1B <- LM5
  CS2B <- LM7
  Vec_CS1 <- LM6 - LM5
  Vec_CS2 <- LM8 - LM7
  if (sqrt(sum(Vec_CS1^2)) < 1e-12) stop("LM5 and LM6 coincide; Vec_CS1 cannot be defined.", call. = FALSE)
  if (sqrt(sum(Vec_CS2^2)) < 1e-12) stop("LM7 and LM8 coincide; Vec_CS2 cannot be defined.", call. = FALSE)

  Anterior_ref <- nrm(Vec_0_2)

  summary_metrics <- c("LM1", "LM2", "LM3", "LM4", "LM1_Line", "CS1B", "Vec_CS1", "CS2B", "Vec_CS2", "Vec_Penp", "Vec_0_2", "Vec_1_1Line", "LM0", "LM6", "LM8", "LM9", "LM10", "LM11")
  summary_mat <- rbind(LM1, LM2, LM3, LM4, LM1_Line, CS1B, Vec_CS1, CS2B, Vec_CS2, Vec_Penp, Vec_0_2, Vec_1_1Line, LM0, LM6, LM8, LM9, LM10, LM11)
  summary_tbl <- data.frame(
    Individual = rep(individual_id, length(summary_metrics)),
    metric = summary_metrics,
    x = round(summary_mat[, 1], 6),
    y = round(summary_mat[, 2], 6),
    z = round(summary_mat[, 3], 6),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  measurements <- data.frame(
    Individual = rep(individual_id, 5),
    metric = c("Corpus_length_LM3_LM9", "Mandibular_length_LM10_LM11", "Estimated_dental_arch_breadth_LM1_LM1_Line", "Dental_arch_superior_length_LM2_LM0", "Estimated_bigonial_breadth_LM9_LM9_Line"),
    value_mm = round(c(dist3(LM3, LM9), dist3(LM10, LM11), dist3(LM1, LM1_Line), dist3(LM2, LM0), dist3(LM9, LM9_Line)), 6),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  manual_orientation <- rbind(
    data.frame(section = "ARP", role = "Point 1", object = "ARP", value = "LM1", x = LM1[1], y = LM1[2], z = LM1[3]),
    data.frame(section = "ARP", role = "Point 2", object = "ARP", value = "LM2", x = LM2[1], y = LM2[2], z = LM2[3]),
    data.frame(section = "ARP", role = "Point 3", object = "ARP", value = "LM1_Line", x = LM1_Line[1], y = LM1_Line[2], z = LM1_Line[3]),
    data.frame(section = "CS1", role = "Plane point", object = "Slice", value = "CS1B/LM5", x = CS1B[1], y = CS1B[2], z = CS1B[3]),
    data.frame(section = "CS1", role = "Vector 1", object = "Slice", value = "Vec_CS1", x = Vec_CS1[1], y = Vec_CS1[2], z = Vec_CS1[3]),
    data.frame(section = "CS1", role = "Vector 2", object = "Slice", value = "Vec_Penp", x = Vec_Penp[1], y = Vec_Penp[2], z = Vec_Penp[3]),
    data.frame(section = "CS2", role = "Plane point", object = "Slice", value = "CS2B/LM7", x = CS2B[1], y = CS2B[2], z = CS2B[3]),
    data.frame(section = "CS2", role = "Vector 1", object = "Slice", value = "Vec_CS2", x = Vec_CS2[1], y = Vec_CS2[2], z = Vec_CS2[3]),
    data.frame(section = "CS2", role = "Vector 2", object = "Slice", value = "Vec_Penp", x = Vec_Penp[1], y = Vec_Penp[2], z = Vec_Penp[3]),
    data.frame(section = "CS3", role = "Plane point", object = "Slice", value = "LM2", x = LM2[1], y = LM2[2], z = LM2[3]),
    data.frame(section = "CS3", role = "Normal", object = "Slice", value = "Vec_1_1Line", x = Vec_1_1Line[1], y = Vec_1_1Line[2], z = Vec_1_1Line[3]),
    data.frame(section = "CS3", role = "Screen-horizontal reference", object = "Camera", value = "Vec_0_2", x = Vec_0_2[1], y = Vec_0_2[2], z = Vec_0_2[3])
  )
  numeric_cols <- vapply(manual_orientation, is.numeric, logical(1))
  manual_orientation[numeric_cols] <- lapply(manual_orientation[numeric_cols], round, 6)

  vectors <- list(Vec_1_1Line = Vec_1_1Line, Vec_0_2 = Vec_0_2, Vec_Penp = Vec_Penp, Vec_CS1 = Vec_CS1, Vec_CS2 = Vec_CS2, Anterior_ref = Anterior_ref)
  points <- list(LM1_Line = LM1_Line, LM9_Line = LM9_Line, LM0 = LM0, CS1B = CS1B, CS2B = CS2B)

  res <- list(
    type = "MANDIBLE",
    individual_id = individual_id,
    landmarks = mat_pts,
    points = points,
    vectors = vectors,
    summary = summary_tbl,
    measurements = measurements,
    manual_orientation = manual_orientation,
    camera_distance_mm = camera_distance_mm,
    cs3_camera_side = cs3_camera_side
  )
  class(res) <- c("mandori_mandible", "mandori_orientation")
  res$avizo_tcl <- avizo_tcl_mandible(res)
  res
}

avizo_tcl_mandible <- function(res) {
  LM <- res$landmarks
  LM1 <- LM["LM1", ]; LM2 <- LM["LM2", ]
  LM1_Line <- res$points$LM1_Line
  Vec_Penp <- res$vectors$Vec_Penp
  Vec_0_2 <- res$vectors$Vec_0_2
  Vec_1_1Line <- res$vectors$Vec_1_1Line
  Anterior_ref <- res$vectors$Anterior_ref
  camdist <- res$camera_distance_mm

  arp_block <- function() {
    emit_plane_3points("ARP", LM1, LM2, LM1_Line, color = c(0, 1, 0), hide_points = TRUE)
  }

  cs12_block <- function(section_label, Psec, Vec_CS) {
    X_screen <- project_vector_to_plane(Vec_CS, Vec_Penp)
    if (sqrt(sum(X_screen^2)) < 1e-12) stop(sprintf("%s: Vec_CS is collinear with Vec_Penp.", section_label), call. = FALSE)
    X_screen <- nrm(X_screen)
    Z_camera <- nrm(cross3(X_screen, Vec_Penp))
    if (dot3(Z_camera, Anterior_ref) < 0) Z_camera <- -Z_camera

    c(
      "# ============================================================",
      sprintf("# MANDIBLE - %s", section_label),
      "# ============================================================",
      "",
      "# ARP clipping plane: 3 points = LM1, LM2, LM1_Line",
      paste(arp_block(), collapse = "\n"),
      "",
      sprintf("# Slice object: point & 2 vectors for %s", section_label),
      paste(emit_point_2vectors_plane("Slice", Psec, Vec_CS, Vec_Penp, hide_points = TRUE), collapse = "\n"),
      "",
      paste(emit_optional_orthogonal_view(Psec, Z_camera, section_label), collapse = "\n"),
      "",
      "# Camera: slice parallel to screen; ARP horizontal; anterior view when applicable",
      paste(emit_camera_from_basis(Psec, Z_camera, X_screen, Y_preferred = Vec_Penp, camDist = camdist), collapse = "\n")
    )
  }

  cs3_block <- function() {
    Psec <- LM2
    Z_camera <- nrm(Vec_1_1Line)
    if (identical(res$cs3_camera_side, "RIGHT")) Z_camera <- -Z_camera
    X_screen <- nrm(Vec_0_2)

    c(
      "# ============================================================",
      "# MANDIBLE - CS3",
      "# ============================================================",
      "",
      "# ARP clipping plane: 3 points = LM1, LM2, LM1_Line",
      paste(arp_block(), collapse = "\n"),
      "",
      "# Slice object: normal & point for CS3",
      paste(emit_slice_normal_point("Slice", Psec, Vec_1_1Line), collapse = "\n"),
      "",
      paste(emit_optional_orthogonal_view(Psec, Z_camera, "CS3"), collapse = "\n"),
      "",
      sprintf("# Camera: slice parallel to screen; ARP horizontal; CS3 side = %s", res$cs3_camera_side),
      paste(emit_camera_from_basis(Psec, Z_camera, X_screen, Y_preferred = Vec_Penp, camDist = camdist), collapse = "\n")
    )
  }

  list(
    CS1 = paste(cs12_block("CS1", res$points$CS1B, res$vectors$Vec_CS1), collapse = "\n"),
    CS2 = paste(cs12_block("CS2", res$points$CS2B, res$vectors$Vec_CS2), collapse = "\n"),
    CS3 = paste(cs3_block(), collapse = "\n")
  )
}
