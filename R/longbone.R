#' Orient long-bone cross-sections for image capture
#'
#' `orient_longbone()` implements the long-bone orientation workflow used by
#' OrientCSG. It combines a longitudinal axis derived from BoneJ Moments of
#' Inertia with a small set of anatomical landmarks to compute cross-sectional
#' locations, anatomical orientation vectors, and Avizo TCL command blocks for
#' automatic section capture.
#'
#' @section Orientation modes:
#' The `mode` argument determines how landmarks are interpreted and how the
#' mediolateral axis is constructed.
#'
#' - `"TIBIA"` uses three landmarks: two plateau landmarks and one tibio-talar
#'   landmark. The midpoint between the plateau landmarks defines the proximal
#'   endpoint of biomechanical length.
#' - `"HUMERUS"` uses four landmarks: two distal landmarks defining the
#'   mediolateral reference direction, one distal landmark for biomechanical
#'   length, and one proximal landmark on the humeral head.
#' - `"HUMERUS_TABLE"` uses two landmarks defining humeral biomechanical length.
#'   The mediolateral direction is derived from the scanner X axis. This mode is
#'   intended for humeri scanned in a standardized table position.
#' 
#'
#' @section Longitudinal axis:
#' The function expects the 3 x 3 eigenvector matrix returned by BoneJ. Following
#' the long-bone protocol, the first column is treated as the longitudinal axis.
#' A coordinate-system correction is then applied to transfer this direction to
#' the Avizo/Amira convention used by the TCL commands. The sign of the
#' longitudinal vector is adjusted when anatomical landmarks provide a distal to
#' proximal reference.
#'
#' @section Section locations:
#' `section_loc` gives the desired section position or positions as percentages
#' of biomechanical length. For example, `section_loc = c(35, 50)` generates TCL
#' blocks named `"SECTION_35"` and `"SECTION_50"`.
#'
#' @section Avizo/Amira requirements:
#' The generated TCL code assumes that the Avizo/Amira project contains one Slice
#' object named `Slice` and two Clipping Plane objects named `ML` and `AP`. The
#' Slice object is used for the transverse section, whereas the `ML` and `AP`
#' planes provide visual anatomical references. All three planes are emitted as
#' normal-and-point definitions in the TCL code for improved compatibility across
#' Amira/Avizo versions.
#'
#' @param mode Character value indicating the orientation mode. Must be one of
#'   `"TIBIA"`, `"HUMERUS"`, or `"HUMERUS_TABLE"`.
#' @param longitudinal_matrix_str Character string containing the 3 x 3 BoneJ
#'   eigenvector matrix. The first column is interpreted as the longitudinal
#'   vector.
#' @param landmarks_str Character string containing landmark coordinates. The
#'   expected number and interpretation of landmarks depend on `mode`.
#' @param section_loc Numeric vector of section locations expressed as
#'   percentages of biomechanical length.
#' @param individual_id Character identifier for the specimen. This value is
#'   copied into the output tables.
#' @param camera_distance_mm Numeric value giving the approximate camera distance
#'   used in the generated Avizo TCL commands or Slicer Python block.
#' @param SOLID Logical. If `TRUE`, the longitudinal axis is computed directly
#'   from `mesh_file` by treating the closed surface mesh as a homogeneous solid.
#' @param SLICER Logical. If `TRUE`, generate 3D Slicer Python command blocks
#'   instead of Avizo TCL blocks. Currently implemented for tibiae.
#' @param mesh_file Optional path to a closed surface mesh (`.ply`, `.stl`, or
#'   `.obj`) used when `SOLID = TRUE`.
#' @param slicer_landmarks_str Optional text block copied from a 3D Slicer
#'   Markups table. Currently implemented for tibiae. Landmark recognition is
#'   positional.
#' @param model_name Optional model node name used by the generated Slicer Python
#'   block. If omitted, the basename of `mesh_file` is used when available.
#' @param landmark_coordinate_system Coordinate system of `slicer_landmarks_str`.
#'   Use `"LPS"` for coordinates exported in the mesh/external convention and
#'   `"RAS"` for coordinates taken directly from Slicer world coordinates.
#'
#' @return An object of class `orientcsg_longbone` and
#'   `orientcsg_orientation`. The object is a list with the following
#'   components:
#'
#'   - `type`: Orientation mode.
#'   - `individual_id`: Specimen identifier.
#'   - `landmarks`: Landmark coordinate matrix.
#'   - `vectors`: Longitudinal, mediolateral, and anteroposterior unit vectors.
#'   - `section_loc`: Requested section percentages.
#'   - `section_points`: Three-dimensional coordinates of each section origin.
#'   - `summary`: Summary table with vectors, landmarks, section points, and
#'     biomechanical length.
#'   - `manual_orientation`: Table arranged for manual verification of
#'     anatomical planes and transverse section orientation in Avizo/Amira.
#'   - `avizo_tcl`: Named list of Avizo TCL command blocks.
#'  
#'
#' @examples
#' \dontrun{
#' longitudinal_matrix_str <- "
#' ||0.008|-0.758|-0.653||
#' ||0.017|-0.652|0.758||
#' ||1.000|0.017|-0.008||
#' "
#'
#' tibia_landmarks_str <- "
#' 130.94606 -12.514749 -392.244507
#' 164.351898 -17.573267 -395.017944
#' 146.258621 -15.388991 -61.599937
#' "
#'
#' res <- orient_longbone(
#'   mode = "TIBIA",
#'   longitudinal_matrix_str = longitudinal_matrix_str,
#'   landmarks_str = tibia_landmarks_str,
#'   section_loc = 50,
#'   individual_id = "TIBIA_001"
#' )
#'
#' View(res$summary)
#' cat(res$avizo_tcl$SECTION_50)
#' }
#'
#' @export
#'
orient_longbone <- function(mode,
                            longitudinal_matrix_str = NULL,
                            landmarks_str = NULL,
                            section_loc = 50,
                            individual_id = "LONG_BONE_001",
                            camera_distance_mm = 300,
                            SOLID = FALSE,
                            SLICER = FALSE,
                            mesh_file = NULL,
                            slicer_landmarks_str = NULL,
                            model_name = NULL,
                            landmark_coordinate_system = "LPS") {
  mode <- toupper(trimws(mode))
  if (!mode %in% c("TIBIA", "HUMERUS", "HUMERUS_TABLE")) {
    stop('`mode` must be one of "TIBIA", "HUMERUS", or "HUMERUS_TABLE".', call. = FALSE)
  }

  if (!is.logical(SOLID) || length(SOLID) != 1L || is.na(SOLID)) {
    stop("`SOLID` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(SLICER) || length(SLICER) != 1L || is.na(SLICER)) {
    stop("`SLICER` must be TRUE or FALSE.", call. = FALSE)
  }

  if (isTRUE(SLICER) && mode != "TIBIA") {
    stop('`SLICER = TRUE` is currently implemented only for `mode = "TIBIA"`.', call. = FALSE)
  }

  section_loc <- as.numeric(section_loc)
  if (any(!is.finite(section_loc)) || any(section_loc < 0 | section_loc > 100)) {
    stop("`section_loc` must contain numeric percentages between 0 and 100.", call. = FALSE)
  }

  mesh_axes <- NULL

  if (isTRUE(SOLID)) {
    if (is.null(mesh_file)) {
      stop("`mesh_file` is required when `SOLID = TRUE`.", call. = FALSE)
    }

    mesh_axes <- compute_mesh_inertia_axes(mesh_file)
    L <- mesh_axes$eigenvectors[, "axis_min_inertia"]
  } else {
    if (is.null(longitudinal_matrix_str)) {
      stop("`longitudinal_matrix_str` is required when `SOLID = FALSE`.", call. = FALSE)
    }

    # BoneJ and Avizo/Amira use different coordinate conventions in this workflow.
    # The diagonal transformation below reproduces the correction used in the
    # original protocol: (x, y, z) -> (-x, -y, z).
    M_bonej <- parse_bonej_eigenvectors(longitudinal_matrix_str)
    T_bonej_to_avizo <- diag(c(-1, -1, 1))
    M_avizo <- T_bonej_to_avizo %*% M_bonej
    L <- M_avizo[, 1]
  }

  n_landmarks <- switch(mode, TIBIA = 3, HUMERUS = 4, HUMERUS_TABLE = 2)

  if (!is.null(slicer_landmarks_str)) {
    if (mode != "TIBIA") {
      stop("`slicer_landmarks_str` is currently implemented only for tibiae.", call. = FALSE)
    }

    mat_pts <- parse_slicer_landmarks(
      slicer_landmarks_str,
      coordinate_system = landmark_coordinate_system
    )
  } else {
    if (is.null(landmarks_str)) {
      stop("`landmarks_str` is required unless `slicer_landmarks_str` is supplied.", call. = FALSE)
    }

    mat_pts <- parse_landmarks(landmarks_str, n_landmarks = n_landmarks, context = mode)
  }

  rownames(mat_pts) <- paste0("P", seq_len(n_landmarks))

  P1 <- mat_pts[1, ]
  P2 <- mat_pts[2, ]
  P3 <- if (mode %in% c("TIBIA", "HUMERUS")) mat_pts[3, ] else NULL
  P4 <- if (mode == "HUMERUS") mat_pts[4, ] else NULL

  Lh <- nrm(L)

  # Use the available anatomical landmarks to ensure that the longitudinal axis
  # points in the expected distal-to-proximal direction whenever this can be
  # checked from the data.
  if (mode == "HUMERUS") {
    long_ref <- P4 - P3
    if (sqrt(sum(long_ref^2)) < 1e-12) {
      stop("LM3 and LM4 coincide; the humeral longitudinal direction cannot be defined.", call. = FALSE)
    }
    if (dot3(long_ref, Lh) < 0) Lh <- -Lh
  } else if (mode == "HUMERUS_TABLE") {
    long_ref <- P2 - P1
    if (sqrt(sum(long_ref^2)) < 1e-12) {
      stop("The distal and proximal landmarks coincide; projected length cannot be defined.", call. = FALSE)
    }
    if (dot3(long_ref, Lh) < 0) Lh <- -Lh
  }

  # Define the mediolateral and anteroposterior axes. For TIBIA and HUMERUS, the
  # initial mediolateral direction comes from landmarks. It is then projected onto
  # the plane perpendicular to L so that the final anatomical axes are strictly
  # orthogonal. In HUMERUS_TABLE mode, the mediolateral direction is derived from
  # the scanner X axis instead.
  if (mode == "HUMERUS_TABLE") {
    X_ref <- c(1, 0, 0)
    ML_proj <- X_ref - dot3(X_ref, Lh) * Lh
    if (sqrt(sum(ML_proj^2)) < 1e-12) {
      stop("The longitudinal vector is collinear with X = (1, 0, 0); ML cannot be defined uniquely.", call. = FALSE)
    }
    MLh <- nrm(ML_proj)
    APh <- nrm(cross3(Lh, MLh))
    MLh <- nrm(cross3(APh, Lh))
  } else {
    cdir <- P2 - P1
    cperp <- cdir - dot3(cdir, Lh) * Lh
    if (sqrt(sum(cperp * cperp)) < 1e-12) {
      stop("P2 - P1 is collinear with L; ML cannot be defined reliably.", call. = FALSE)
    }
    MLh <- nrm(cperp)
    APh <- nrm(cross3(Lh, MLh))
    MLh <- nrm(cross3(APh, Lh))
    if (mode == "HUMERUS") {
      MLh <- -MLh
      APh <- -APh
    }
  }

  projected <- list()

  # Compute biomechanical length and the origin of each requested section. The
  # distance is always measured parallel to the longitudinal axis, following the
  # logic of the original long-bone protocol.
  if (mode == "TIBIA") {
    Midpoint <- (P1 + P2) / 2
    Proj_LM3 <- P3
    Proj_Mid <- P3 + dot3(Midpoint - P3, Lh) * Lh
    Bio_vec <- Proj_Mid - Proj_LM3
    Bio_length <- sqrt(sum(Bio_vec^2))
    point_at_pct <- function(pct) Proj_LM3 + (pct / 100) * Bio_vec
    projected$Proj_TibioTalar <- Proj_LM3
    projected$Proj_Midpoint <- Proj_Mid
  } else if (mode == "HUMERUS") {
    Proj_LM3 <- P3
    Proj_LM4 <- P3 + dot3(P4 - P3, Lh) * Lh
    Bio_vec <- Proj_LM4 - Proj_LM3
    Bio_length <- sqrt(sum(Bio_vec^2))
    point_at_pct <- function(pct) Proj_LM3 + (pct / 100) * Bio_vec
    projected$Proj_LM3 <- Proj_LM3
    projected$Proj_LM4 <- Proj_LM4
  } else {
    Bio_length <- abs(dot3(P2 - P1, Lh))
    point_at_pct <- function(pct) P1 + (pct / 100) * Bio_length * Lh
  }

  if (Bio_length < 1e-12) {
    stop("The projected biomechanical length is near zero.", call. = FALSE)
  }

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
    x = round(sx, 6),
    y = round(sy, 6),
    z = round(sz, 6),
    Bio_length = round(c(Bio_length, rep(NA_real_, length(summary_metrics) - 1)), 6),
    check.names = FALSE,
    stringsAsFactors = FALSE
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

  if (is.null(model_name) || length(model_name) != 1L || is.na(model_name) || !nzchar(model_name)) {
    if (!is.null(mesh_file)) {
      model_name <- tools::file_path_sans_ext(basename(mesh_file))
    } else {
      model_name <- "Segment_1_solid"
    }
  }

  res <- list(
    type = mode,
    individual_id = individual_id,
    landmarks = mat_pts,
    vectors = list(L = Lh, ML = MLh, AP = APh),
    section_loc = section_loc,
    section_points = section_points,
    summary = summary_tbl,
    manual_orientation = manual_orientation,
    camera_distance_mm = camera_distance_mm,
    SOLID = SOLID,
    SLICER = SLICER,
    mesh_file = mesh_file,
    model_name = model_name,
    mesh_axes = mesh_axes,
    projected = projected
  )
  class(res) <- c("orientcsg_longbone", "orientcsg_orientation")

  if (isTRUE(SLICER)) {
    res$slicer_py <- lapply(names(res$section_points), function(sec) {
      emit_slicer_section_python(res, section = sec)
    })
    names(res$slicer_py) <- names(res$section_points)
  } else {
    res$avizo_tcl <- avizo_tcl_longbone(res)
  }

  res
}

# Internal long-bone camera helper ------------------------------------------
#
# Orient the camera so that the section is parallel to the screen and the
# anatomical axes appear in a consistent orientation. Tibial sections are handled
# with a sign convention that matches the established capture protocol.
emit_longbone_camera <- function(P, L, ML, AP, mode, camDist = 300) {
  Lc <- nrm(L)
  Lref <- nrm(c(0, 0, -1))
  if (dot3(Lc, Lref) < 0) Lc <- -Lc

  MLc <- ML - dot3(ML, Lc) * Lc
  if (sqrt(sum(MLc^2)) < 1e-12) {
    stop("ML is invalid after projection relative to L.", call. = FALSE)
  }
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

# Internal long-bone TCL generator ------------------------------------------
#
# Convert a long-bone orientation result into one Avizo TCL block per requested
# section. Each block repositions the Slice object, updates the ML and AP visual
# planes, and sets the camera for standardized image capture. The Slice, ML, and
# AP planes are emitted as normal-and-point definitions for better compatibility
# across Amira/Avizo versions.
avizo_tcl_longbone <- function(res) {
  Lh <- res$vectors$L
  MLh <- res$vectors$ML
  APh <- res$vectors$AP
  out <- list()

  for (i in seq_along(res$section_points)) {
    key <- names(res$section_points)[i]
    label <- res$section_loc[i]
    Psec <- res$section_points[[i]]
    ML_normal <- nrm(cross3(Lh, MLh))
    AP_normal <- nrm(cross3(Lh, APh))

    block <- c(
      "# ============================================================",
      sprintf("# SECTION %s%%", label),
      "# ============================================================",
      "",
      paste(emit_normal_point_plane("Slice", Psec, Lh), collapse = "\n"),
      "",
      "# ML visual plane: normal & point",
      paste(emit_normal_point_plane("ML", Psec, ML_normal, color = c(0, 1, 0), hide_points = TRUE), collapse = "\n"),
      "",
      "# AP visual plane: normal & point",
      paste(emit_normal_point_plane("AP", Psec, AP_normal, color = c(0, 0, 1), hide_points = TRUE), collapse = "\n"),
      "",
      paste(emit_longbone_camera(Psec, Lh, MLh, APh, mode = res$type, camDist = res$camera_distance_mm), collapse = "\n")
    )
    out[[key]] <- paste(block, collapse = "\n")
  }

  out
}
