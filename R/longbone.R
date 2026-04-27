#' Orient long bone cross-sections
#'
#' @param mode One of "TIBIA", "HUMERUS", or "HUMERUS_TABLE".
#' @param longitudinal_matrix_str BoneJ eigenvector matrix string.
#' @param landmarks_str Landmark coordinates string.
#' @param section_loc Numeric vector of section percentages.
#' @param individual_id Specimen identifier.
#' @param camera_distance_mm Approximate camera distance in Avizo units/mm.
#' @return A `orientcsg_orientation` object.
#' @export
orient_longbone <- function(mode,
                            longitudinal_matrix_str,
                            landmarks_str,
                            section_loc = 50,
                            individual_id = "LONG_BONE_001",
                            camera_distance_mm = 300) {
  mode <- toupper(trimws(mode))
  if (!mode %in% c("TIBIA", "HUMERUS", "HUMERUS_TABLE")) {
    stop('`mode` must be one of "TIBIA", "HUMERUS", or "HUMERUS_TABLE".', call. = FALSE)
  }
  section_loc <- as.numeric(section_loc)
  if (any(!is.finite(section_loc)) || any(section_loc < 0 | section_loc > 100)) {
    stop("`section_loc` must contain numeric percentages between 0 and 100.", call. = FALSE)
  }

  M_bonej <- parse_bonej_eigenvectors(longitudinal_matrix_str)
  T_bonej_to_avizo <- diag(c(-1, -1, 1))
  M_avizo <- T_bonej_to_avizo %*% M_bonej
  L <- M_avizo[, 1]

  n_landmarks <- switch(mode, TIBIA = 3, HUMERUS = 4, HUMERUS_TABLE = 2)
  mat_pts <- parse_landmarks(landmarks_str, n_landmarks = n_landmarks, context = mode)
  rownames(mat_pts) <- paste0("P", seq_len(n_landmarks))
  P1 <- mat_pts[1, ]; P2 <- mat_pts[2, ]
  P3 <- if (mode %in% c("TIBIA", "HUMERUS")) mat_pts[3, ] else NULL
  P4 <- if (mode == "HUMERUS") mat_pts[4, ] else NULL

  Lh <- nrm(L)

  if (mode == "HUMERUS") {
    long_ref <- P4 - P3
    if (sqrt(sum(long_ref^2)) < 1e-12) stop("LM3 and LM4 coincide; the humeral longitudinal direction cannot be defined.", call. = FALSE)
    if (dot3(long_ref, Lh) < 0) Lh <- -Lh
  } else if (mode == "HUMERUS_TABLE") {
    long_ref <- P2 - P1
    if (sqrt(sum(long_ref^2)) < 1e-12) stop("The distal and proximal landmarks coincide; projected length cannot be defined.", call. = FALSE)
    if (dot3(long_ref, Lh) < 0) Lh <- -Lh
  }

  if (mode == "HUMERUS_TABLE") {
    X_ref <- c(1, 0, 0)
    ML_proj <- X_ref - dot3(X_ref, Lh) * Lh
    if (sqrt(sum(ML_proj^2)) < 1e-12) stop("The longitudinal vector is collinear with X = (1, 0, 0); ML cannot be defined uniquely.", call. = FALSE)
    MLh <- nrm(ML_proj)
    APh <- nrm(cross3(Lh, MLh))
    MLh <- nrm(cross3(APh, Lh))
  } else {
    cdir <- P2 - P1
    cperp <- cdir - dot3(cdir, Lh) * Lh
    if (sqrt(sum(cperp * cperp)) < 1e-12) stop("P2 - P1 is collinear with L; ML cannot be defined reliably.", call. = FALSE)
    MLh <- nrm(cperp)
    APh <- nrm(cross3(Lh, MLh))
    MLh <- nrm(cross3(APh, Lh))
    if (mode == "HUMERUS") {
      MLh <- -MLh
      APh <- -APh
    }
  }

  if (mode == "TIBIA") {
    Midpoint <- (P1 + P2) / 2
    Proj_LM3 <- P3
    Proj_Mid <- P3 + dot3(Midpoint - P3, Lh) * Lh
    Bio_vec <- Proj_Mid - Proj_LM3
    Bio_length <- sqrt(sum(Bio_vec^2))
    point_at_pct <- function(pct) Proj_LM3 + (pct / 100) * Bio_vec
  } else if (mode == "HUMERUS") {
    Proj_LM3 <- P3
    Proj_LM4 <- P3 + dot3(P4 - P3, Lh) * Lh
    Bio_vec <- Proj_LM4 - Proj_LM3
    Bio_length <- sqrt(sum(Bio_vec^2))
    point_at_pct <- function(pct) Proj_LM3 + (pct / 100) * Bio_vec
  } else {
    Bio_length <- abs(dot3(P2 - P1, Lh))
    point_at_pct <- function(pct) P1 + (pct / 100) * Bio_length * Lh
  }

  if (Bio_length < 1e-12) stop("The projected biomechanical length is near zero.", call. = FALSE)
  section_points <- lapply(section_loc, point_at_pct)
  names(section_points) <- paste0("SECTION_", section_loc)

  point_metric_names <- paste0("Point_", section_loc, "%")
  point_x <- vapply(section_points, function(v) v[1], numeric(1))
  point_y <- vapply(section_points, function(v) v[2], numeric(1))
  point_z <- vapply(section_points, function(v) v[3], numeric(1))

  if (mode == "TIBIA") {
    labels <- c("Plateau1", "Plateau2", "TibioTalar")
    summary_metrics <- c("Long_Vector", "ML", "AP", labels, point_metric_names)
    sx <- c(Lh[1], MLh[1], APh[1], P1[1], P2[1], P3[1], point_x)
    sy <- c(Lh[2], MLh[2], APh[2], P1[2], P2[2], P3[2], point_y)
    sz <- c(Lh[3], MLh[3], APh[3], P1[3], P2[3], P3[3], point_z)
  } else if (mode == "HUMERUS") {
    labels <- c("MedialTrocleaAnt", "CapitulumAnt", "LateralTrocleaDist", "Proximal Head")
    summary_metrics <- c("Long_Vector", "ML", "AP", labels, point_metric_names)
    sx <- c(Lh[1], MLh[1], APh[1], P1[1], P2[1], P3[1], P4[1], point_x)
    sy <- c(Lh[2], MLh[2], APh[2], P1[2], P2[2], P3[2], P4[2], point_y)
    sz <- c(Lh[3], MLh[3], APh[3], P1[3], P2[3], P3[3], P4[3], point_z)
  } else {
    labels <- c("LateralTrocleaDist", "Proximal Head")
    summary_metrics <- c("Long_Vector", "ML", "AP", labels, point_metric_names)
    sx <- c(Lh[1], MLh[1], APh[1], P1[1], P2[1], point_x)
    sy <- c(Lh[2], MLh[2], APh[2], P1[2], P2[2], point_y)
    sz <- c(Lh[3], MLh[3], APh[3], P1[3], P2[3], point_z)
  }

  summary_tbl <- data.frame(
    Individual = rep(individual_id, length(summary_metrics)),
    metric = summary_metrics,
    x = round(sx, 6), y = round(sy, 6), z = round(sz, 6),
    Bio_length = round(c(Bio_length, rep(NA_real_, length(summary_metrics) - 1)), 6),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  manual_orientation <- rbind(
    data.frame(plane = "coronal", vector = "L_longitudinal", x = Lh[1], y = Lh[2], z = Lh[3]),
    data.frame(plane = "coronal", vector = "ML_mediolateral", x = MLh[1], y = MLh[2], z = MLh[3]),
    data.frame(plane = "sagittal", vector = "L_longitudinal", x = Lh[1], y = Lh[2], z = Lh[3]),
    data.frame(plane = "sagittal", vector = "AP_anteroposterior", x = APh[1], y = APh[2], z = APh[3]),
    data.frame(plane = "transverse", vector = "L_longitudinal", x = Lh[1], y = Lh[2], z = Lh[3])
  )
  numeric_cols <- vapply(manual_orientation, is.numeric, logical(1))
  manual_orientation[numeric_cols] <- lapply(manual_orientation[numeric_cols], round, 6)

  res <- list(
    type = mode,
    individual_id = individual_id,
    landmarks = mat_pts,
    vectors = list(L = Lh, ML = MLh, AP = APh),
    section_loc = section_loc,
    section_points = section_points,
    summary = summary_tbl,
    manual_orientation = manual_orientation,
    camera_distance_mm = camera_distance_mm
  )
  class(res) <- c("orientcsg_longbone", "orientcsg_orientation")
  res$avizo_tcl <- avizo_tcl_longbone(res)
  res
}

emit_longbone_camera <- function(P, L, ML, AP, mode, camDist = 300) {
  Lc <- nrm(L)
  Lref <- nrm(c(0, 0, -1))
  if (dot3(Lc, Lref) < 0) Lc <- -Lc
  MLc <- ML - dot3(ML, Lc) * Lc
  if (sqrt(sum(MLc^2)) < 1e-12) stop("ML is invalid after projection relative to L.", call. = FALSE)
  MLc <- nrm(MLc)
  APc <- nrm(cross3(Lc, MLc))
  MLc <- nrm(cross3(APc, Lc))
  APref <- nrm(c(0, -1, 0))
  if (dot3(APc, APref) < 0) {
    APc <- -APc
    MLc <- -MLc
  }

  if (mode == "TIBIA") {
    emit_camera_from_basis(P, Z_axis = Lc, X_axis = -MLc, Y_preferred = -APc, camDist = camDist)
  } else {
    emit_camera_from_basis(P, Z_axis = Lc, X_axis = MLc, Y_preferred = APc, camDist = camDist)
  }
}

avizo_tcl_longbone <- function(res) {
  Lh <- res$vectors$L; MLh <- res$vectors$ML; APh <- res$vectors$AP
  out <- list()
  for (i in seq_along(res$section_points)) {
    key <- names(res$section_points)[i]
    label <- res$section_loc[i]
    Psec <- res$section_points[[i]]

    block <- c(
      "# ============================================================",
      sprintf("# SECTION %s%%", label),
      "# ============================================================",
      "",
      paste(emit_slice_normal_point("Slice", Psec, Lh), collapse = "\n"),
      "",
      paste(emit_point_2vectors_plane("ML", Psec, Lh, MLh, color = c(0, 1, 0), hide_points = TRUE), collapse = "\n"),
      "",
      paste(emit_point_2vectors_plane("AP", Psec, Lh, APh, color = c(0, 0, 1), hide_points = TRUE), collapse = "\n"),
      "",
      paste(emit_longbone_camera(Psec, Lh, MLh, APh, mode = res$type, camDist = res$camera_distance_mm), collapse = "\n")
    )
    out[[key]] <- paste(block, collapse = "\n")
  }
  out
}
