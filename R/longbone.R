#' Orient long-bone cross-sections and generate capture scripts
#'
#' `orient_longbone()` implements the long-bone orientation workflows used by
#' OrientCSG. It computes biomechanical length, cross-sectional locations, and
#' anatomical orientation vectors for tibiae and humeri from a small set of
#' anatomical landmarks plus either a BoneJ Moments of Inertia eigenvector matrix
#' or a closed surface mesh. The function can generate Avizo/Amira TCL command
#' blocks for the classic CT-derived workflow or 3D Slicer Python command blocks
#' for workflows based on solid surface meshes.
#'
#' @section Input workflows:
#' The `SOLID` argument controls how the longitudinal axis is obtained.
#'
#' - `SOLID = FALSE` implements the classic DICOM/CT workflow. In this mode,
#'   `longitudinal_matrix_str` must contain the 3 x 3 BoneJ Moments of Inertia
#'   eigenvector matrix, and `dicom_iop` should contain the DICOM Image
#'   Orientation (Patient) field for the image stack used in BoneJ. The first
#'   column of the BoneJ matrix is interpreted as the longitudinal axis after
#'   conversion from the ImageJ/BoneJ stack basis to the internal DICOM/LPS
#'   convention.
#' - `SOLID = TRUE` implements the solid-mesh workflow. In this mode, `mesh_file`
#'   must point to a watertight `.ply`, `.stl`, or `.obj` surface mesh. The mesh
#'   is treated as a homogeneous closed solid, and the eigenvector associated
#'   with the smallest principal moment of inertia is used as the longitudinal
#'   axis. This workflow requires the suggested package Rvcg.
#'
#' @section Output backends:
#' The `SLICER` argument controls the type of capture script returned.
#'
#' - `SLICER = FALSE` returns Avizo/Amira TCL blocks. This is the default backend
#'   and is compatible with `mode = "TIBIA"`, `mode = "HUMERUS"`, and
#'   `mode = "HUMERUS_TABLE"`.
#' - `SLICER = TRUE` returns 3D Slicer Python blocks. This backend is implemented
#'   for `mode = "TIBIA"` and `mode = "HUMERUS"`. It is intentionally not
#'   implemented for `mode = "HUMERUS_TABLE"`, because surface scans do not
#'   preserve a reliable scanner/table orientation.
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
#'   intended for humeri scanned in a standardized table position and is only
#'   available for Avizo/Amira TCL output.
#'
#' @section Longitudinal axis:
#' When `SOLID = FALSE`, the function expects the 3 x 3 eigenvector matrix
#' returned by BoneJ. Following the long-bone protocol, the first column is
#' treated as the longitudinal axis. By default, the BoneJ matrix is transformed
#' using the DICOM Image Orientation (Patient) field supplied through
#' `dicom_iop`; this replaces the earlier fixed `(-x, -y, z)` correction and
#' supports stacks with different DICOM orientations. When `SOLID = TRUE`, the
#' longitudinal axis is estimated directly from the closed mesh by volumetric
#' inertia. In both cases, the sign of the longitudinal vector is adjusted when
#' anatomical landmarks provide a distal-to-proximal reference. For tibiae, this
#' reference is defined from the tibio-talar landmark to the midpoint of the two
#' plateau landmarks.
#'
#' @section Section locations:
#' `section_loc` gives the desired section position or positions as percentages
#' of biomechanical length. For example, `section_loc = c(35, 50)` generates
#' script blocks named `"SECTION_35"` and `"SECTION_50"`.
#'
#' @section Avizo/Amira requirements:
#' When `SLICER = FALSE`, the generated TCL code assumes that the Avizo/Amira
#' project contains one Slice object named `Slice` and two Clipping Plane objects
#' named `ML` and `AP`. The Slice object is used for the transverse section,
#' whereas the `ML` and `AP` planes provide visual anatomical references. All
#' three planes are emitted as normal-and-point definitions in the TCL code for
#' improved compatibility across Amira/Avizo versions.
#'
#' @section 3D Slicer requirements:
#' When `SLICER = TRUE`, the generated Python code assumes that the corresponding
#' model is loaded in 3D Slicer. If `model_name` is omitted, the function uses the
#' basename of `mesh_file` when available. Landmark rows copied from a Slicer
#' Markups table can be supplied through `landmarks_str`; however, the coordinate
#' system must describe the numeric values that are actually pasted into R. In
#' common Slicer Markups table/export workflows these pasted values may be LPS,
#' even though the Slicer interface displays R/A/S columns. Use
#' `lm_coord_system = "LPS"` for such copied/exported text. Use
#' `lm_coord_system = "RAS"` only for values that are confirmed to be true Slicer
#' world RAS coordinates, for example values extracted with
#' `GetNthControlPointPositionWorld()`. The older arguments
#' `slicer_landmarks_str` and `landmark_coordinate_system` are retained as
#' aliases for backward compatibility.
#'
#' @param mode Character value indicating the orientation mode. Must be one of
#'   `"TIBIA"`, `"HUMERUS"`, or `"HUMERUS_TABLE"`.
#' @param longitudinal_matrix_str Character string containing the 3 x 3 BoneJ
#'   eigenvector matrix. Required when `SOLID = FALSE`. The first column is
#'   interpreted as the longitudinal vector.
#' @param dicom_iop Optional DICOM Image Orientation (Patient) information for
#'   the stack used in BoneJ. Usually this is pasted directly as the full DICOM
#'   line, for example
#'   `r"(0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0)"`.
#'   Required by default when `SOLID = FALSE`.
#' @param landmarks_str Character string containing landmark coordinates. The
#'   expected number and interpretation of landmarks depend on `mode`. Plain XYZ
#'   coordinates and Slicer Markups-style rows are both accepted.
#' @param section_loc Numeric vector of section locations expressed as
#'   percentages of biomechanical length.
#' @param individual_id Character identifier for the specimen. This value is
#'   copied into the output tables.
#' @param camera_distance_mm Numeric value giving the approximate camera distance
#'   used in the generated Avizo TCL commands or Slicer Python block.
#' @param SOLID Logical. If `TRUE`, the longitudinal axis is computed directly
#'   from `mesh_file` by treating a watertight closed surface mesh as a
#'   homogeneous solid. If `FALSE`, the longitudinal axis is read from
#'   `longitudinal_matrix_str`.
#' @param SLICER Logical. If `TRUE`, generate 3D Slicer Python command blocks.
#'   If `FALSE`, generate Avizo/Amira TCL blocks. Slicer output is currently
#'   implemented for tibiae and humeri; `HUMERUS_TABLE` is intentionally not
#'   supported for Slicer output.
#' @param mesh_file Optional path to a watertight closed surface mesh (`.ply`,
#'   `.stl`, or `.obj`) used when `SOLID = TRUE`.
#' @param lm_coord_system Coordinate system of the numeric landmark values pasted
#'   into R. The default is `"LPS"`, matching the Avizo/Amira-like internal
#'   convention. Coordinates copied or exported from 3D Slicer Markups may paste
#'   as LPS even when the Slicer table displays R/A/S columns; in that case keep
#'   `lm_coord_system = "LPS"`. Use `"RAS"` only for values that are actually RAS,
#'   such as coordinates extracted from Slicer with
#'   `GetNthControlPointPositionWorld()`. This argument controls the spatial
#'   interpretation of the numbers only; it does not depend on whether the text
#'   was pasted as plain XYZ coordinates or as a Slicer Markups-style table.
#' @param slicer_landmarks_str Deprecated alias for `landmarks_str`, retained so
#'   older scripts continue to run with the updated parser and coordinate logic.
#' @param model_name Optional model node name used by the generated Slicer Python
#'   block. If omitted, the basename of `mesh_file` is used when available.
#' @param landmark_coordinate_system Deprecated alias for `lm_coord_system`,
#'   retained for backward compatibility.
#' @param bonej_coord_transform Advanced character argument controlling how the
#'   BoneJ eigenvector matrix is transformed before use. The default,
#'   `"dicom_iop"`, derives the transformation from `dicom_iop`. The values
#'   `"legacy_flip_xy"`, `"none"`, and `"manual"` are retained for diagnostic
#'   or backward-compatible workflows.
#' @param bonej_transform_matrix Optional 3 x 3 matrix used only when
#'   `bonej_coord_transform = "manual"`.
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
#'   - `avizo_tcl`: Named list of Avizo TCL command blocks, when `SLICER = FALSE`.
#'   - `slicer_py`: Named list of 3D Slicer Python command blocks, when
#'     `SLICER = TRUE`.
#'   - `mesh_axes`: Mesh-derived inertia information, when `SOLID = TRUE`.
#'   - `bonej`: BoneJ eigenvector and coordinate-transform information, when
#'     `SOLID = FALSE`.
#'   - `longitudinal_axis_check`: Axial angle between the transformed
#'     longitudinal vector and the anatomical distal-proximal reference.
#'
#' @examples
#' \dontrun{
#' dicom_iop_str <- r"(0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0)"
#'
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
#'   dicom_iop = dicom_iop_str,
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
                            dicom_iop = NULL,
                            landmarks_str = NULL,
                            section_loc = 50,
                            individual_id = "LONG_BONE_001",
                            camera_distance_mm = 300,
                            SOLID = FALSE,
                            SLICER = FALSE,
                            mesh_file = NULL,
                            lm_coord_system = "LPS",
                            slicer_landmarks_str = NULL,
                            model_name = NULL,
                            landmark_coordinate_system = NULL,
                            bonej_coord_transform = "dicom_iop",
                            bonej_transform_matrix = NULL) {
  lm_coord_system_missing <- missing(lm_coord_system)
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

  if (isTRUE(SLICER) && mode == "HUMERUS_TABLE") {
    stop('`SLICER = TRUE` is not implemented for `mode = "HUMERUS_TABLE"`.', call. = FALSE)
  }

  if (isTRUE(SLICER) && !mode %in% c("TIBIA", "HUMERUS")) {
    stop('`SLICER = TRUE` is currently implemented only for `mode = "TIBIA"` or `mode = "HUMERUS"`.', call. = FALSE)
  }

  lm_coord_system <- resolve_lm_coord_system(
    lm_coord_system = lm_coord_system,
    landmark_coordinate_system = landmark_coordinate_system,
    lm_coord_system_missing = lm_coord_system_missing
  )

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

    # BoneJ returns vectors in the ImageJ stack basis. For DICOM-derived stacks,
    # this basis is not always related to the Avizo/Amira/DICOM patient basis by
    # the same fixed sign change. By default, the transformation is derived from
    # DICOM Image Orientation (Patient), while legacy and manual options remain
    # available for reproducibility and diagnostics.
    M_bonej <- parse_bonej_eigenvectors(longitudinal_matrix_str)
    bonej_transform <- resolve_bonej_transform(
      bonej_coord_transform = bonej_coord_transform,
      dicom_iop = dicom_iop,
      bonej_transform_matrix = bonej_transform_matrix
    )
    M_internal <- bonej_transform$matrix %*% M_bonej
    L <- M_internal[, 1]
  }

  n_landmarks <- switch(mode, TIBIA = 3, HUMERUS = 4, HUMERUS_TABLE = 2)

  landmarks_str <- resolve_landmarks_str(
    landmarks_str = landmarks_str,
    slicer_landmarks_str = slicer_landmarks_str
  )

  mat_pts <- parse_landmarks(landmarks_str, n_landmarks = n_landmarks, context = mode)
  mat_pts <- normalize_lm_coordinates(mat_pts, lm_coord_system = lm_coord_system)
  rownames(mat_pts) <- paste0("P", seq_len(n_landmarks))

  P1 <- mat_pts[1, ]
  P2 <- mat_pts[2, ]
  P3 <- if (mode %in% c("TIBIA", "HUMERUS")) mat_pts[3, ] else NULL
  P4 <- if (mode == "HUMERUS") mat_pts[4, ] else NULL

  Lh <- nrm(L)

  # Use the available anatomical landmarks to ensure that the longitudinal axis
  # points in the expected distal-to-proximal direction whenever this can be
  # checked from the data.
  if (mode == "TIBIA") {
    long_ref <- ((P1 + P2) / 2) - P3
    if (sqrt(sum(long_ref^2)) < 1e-12) {
      stop("The tibio-talar landmark and plateau midpoint coincide; the tibial longitudinal direction cannot be defined.", call. = FALSE)
    }
    if (dot3(long_ref, Lh) < 0) Lh <- -Lh
  } else if (mode == "HUMERUS") {
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

  longitudinal_axis_check <- longbone_axis_check(mode, mat_pts, Lh)
  if (!is.null(longitudinal_axis_check) &&
      is.finite(longitudinal_axis_check$angle_deg) &&
      longitudinal_axis_check$angle_deg > longitudinal_axis_check$warning_threshold_deg) {
    warning(
      sprintf(
        "The transformed BoneJ longitudinal axis is %.2f degrees away from the anatomical %s reference. Check `dicom_iop`, slice order, or `bonej_coord_transform`.",
        longitudinal_axis_check$angle_deg,
        longitudinal_axis_check$reference
      ),
      call. = FALSE
    )
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
    lm_coord_system = lm_coord_system,
    internal_coord_system = "LPS",
    SOLID = SOLID,
    SLICER = SLICER,
    mesh_file = mesh_file,
    model_name = model_name,
    mesh_axes = mesh_axes,
    bonej = if (isTRUE(SOLID)) NULL else list(
      coord_transform = bonej_transform$name,
      dicom_iop = bonej_transform$dicom_iop,
      transform_matrix = bonej_transform$matrix,
      eigenvectors = M_bonej,
      transformed_eigenvectors = M_internal
    ),
    longitudinal_axis_check = longitudinal_axis_check,
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

# Internal long-bone diagnostic helpers -------------------------------------
#
# Compare the transformed BoneJ longitudinal axis with an anatomical long-axis
# reference derived from the supplied landmarks. The comparison is axial, so the
# sign of the eigenvector is ignored.
axis_angle_deg <- function(a, b) {
  a <- nrm(a)
  b <- nrm(b)
  d <- abs(dot3(a, b))
  d <- max(-1, min(1, d))
  acos(d) * 180 / pi
}

longbone_axis_check <- function(mode, mat_pts, L, warning_threshold_deg = 15) {
  if (mode == "TIBIA") {
    ref <- ((mat_pts[1, ] + mat_pts[2, ]) / 2) - mat_pts[3, ]
    ref_label <- "tibio-talar to plateau-midpoint"
  } else if (mode == "HUMERUS") {
    ref <- mat_pts[4, ] - mat_pts[3, ]
    ref_label <- "distal-to-proximal humeral"
  } else if (mode == "HUMERUS_TABLE") {
    ref <- mat_pts[2, ] - mat_pts[1, ]
    ref_label <- "distal-to-proximal humeral-table"
  } else {
    return(NULL)
  }

  if (sqrt(sum(ref^2)) < 1e-12) return(NULL)

  list(
    reference = ref_label,
    reference_vector = nrm(ref),
    angle_deg = axis_angle_deg(L, ref),
    warning_threshold_deg = warning_threshold_deg
  )
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
