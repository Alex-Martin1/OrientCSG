#' Orient mandibular cross-sections for image capture
#'
#' `orient_mandible()` implements the mandibular orientation workflow used by
#' OrientCSG. It receives the coordinates of the mandibular landmarks defined in
#' the protocol, reconstructs the geometric reference system, computes the three
#' planned cross-sections, and returns tabular results plus software-specific
#' command blocks for automatic orientation in Avizo/Amira or 3D Slicer.
#'
#' @section Landmark order and preservation modes:
#' The function accepts 9, 11, or 12 landmarks, pasted as a single coordinate
#' string. The coordinates must be supplied in the fixed order used by the
#' mandibular protocol. Each landmark must contain three coordinates (`x`, `y`,
#' `z`). The function does not infer landmark identity from anatomical position;
#' it assumes that the order of the input string is correct.
#'
#' The 11-landmark input preserves the original workflow and assumes that
#' `LM1`, `LM2`, ..., `LM11` are available. The 12-landmark input adds `LM12`,
#' interpreted as the contralateral gonion, allowing direct computation of
#' bigonial breadth as `LM9`--`LM12` when `LM9` is anatomically valid. The
#' 9-landmark input is intended for specimens where `LM10` and `LM11` cannot be
#' placed; in that case mandibular length is returned as non-computable.
#'
#' @section Complete-arch mode:
#' By default, `complete_arch = FALSE`, and `LM1_Line` is estimated by reflecting
#' `LM1` across the plane defined by `LM2`, `LM3`, and `LM4`, matching the
#' original fragmented-mandible workflow. When `complete_arch = TRUE`, `LM4` is
#' interpreted as the physically preserved `LM1_Line`/`A_Line` point. In this
#' mode, dental arch breadth is computed directly as `LM1`--`LM4`, and the
#' reflection plane used for estimated contralateral points is rebuilt from the
#' complete arch.
#'
#' @section Geometric procedure:
#' The alveolar reference plane (ARP) is defined from `LM1`, `LM2`, and
#' `LM1_Line`. This plane provides the reference system used to orient all
#' mandibular cross-sections. Cross-section 1 (`CS1`) is defined from the
#' direction `LM5 -> LM6` and is made perpendicular to the ARP. Cross-section 2
#' (`CS2`) follows the same logic, using the direction `LM7 -> LM8`.
#' Cross-section 3 (`CS3`) is defined by a point and a normal: it passes through
#' `LM2` and uses the `LM1 -> LM1_Line` direction as its normal.
#'
#' The function also computes auxiliary points and vectors, including `LM0`,
#' `ARP_Origin`, `Vec_1_1Line`, `Vec_Penp`, `Vec_CS1_Normal`, and
#' `Vec_CS2_Normal`, because these elements are needed for manual verification,
#' image orientation, and size-related measurements. `Vec_CS1` and `Vec_CS2` are
#' retained internally for TCL and camera generation, but are not included in the
#' compact summary table. The sign of `Vec_Penp` is selected anatomically so
#' that it points from inferior toward superior. The priority is: real `LM9` when
#' `lm9_valid = TRUE`; `LM3`/`LM4` as inferior references when suitable for the
#' selected protocol; and an orientation-only `LM9` placeholder when `lm9_valid =
#' FALSE`. This same signed vector controls the screen-vertical orientation in
#' both Avizo/Amira and 3D Slicer outputs.
#'
#' @section Avizo/Amira and 3D Slicer backends:
#' With the default `SLICER = FALSE`, the function returns Avizo/Amira TCL blocks.
#' The generated TCL code assumes that the Avizo project contains objects named
#' `ARP` and `Slice`. An optional object named `OrthogonalView` can be used as a
#' visual check plane, but the TCL code is written so that it will still run if
#' this object does not exist.
#'
#' With `SLICER = TRUE`, the function returns 3D Slicer Python blocks. These
#' blocks orient the Red slice view to the requested mandibular section, emit RAS
#' coordinates in the Python interactor, activate volume rendering with the
#' `CT-AAA2` preset when available, create the ARP and `LM1_Line` verification
#' objects, add a 10 mm scale bar, and configure a 3D verification view. The
#' generated block also defines `restore_view()` and
#' `refresh_orientcsg_scale()` helper commands.
#'
#' @param landmarks_str Character string containing the coordinates of 9, 11, or
#'   12 mandibular landmarks in the fixed protocol order. Plain XYZ coordinates
#'   and Slicer Markups-style rows are both accepted. Values may be separated by
#'   spaces, tabs, commas, semicolons, or vertical bars.
#' @param individual_id Character identifier for the specimen. This value is
#'   copied into the output tables.
#' @param camera_distance_mm Numeric value giving the approximate camera distance
#'   used in the generated Avizo TCL commands. The default is `300`, which is a
#'   practical value for the current image-capture workflow.
#' @param lm1_side Character value indicating the anatomical side on which `LM1`
#'   was placed. Allowed values are `"RIGHT"` and `"LEFT"`. This argument
#'   controls the side from which CS1 and CS2 are viewed and the anatomical
#'   interpretation of the transverse axis used for CS3.
#' @param complete_arch Logical. If `FALSE` (default), `LM1_Line` is estimated by
#'   reflection across the plane defined by `LM2`, `LM3`, and `LM4`. If `TRUE`,
#'   `LM4` is interpreted as the physically preserved `LM1_Line`/`A_Line` point.
#' @param estimate_lm10 Logical. If `FALSE` (default), mandibular length is
#'   computed directly from `LM10` to `LM11` when both landmarks are available. If
#'   `TRUE`, `LM10` is reflected across the relevant symmetry plane and
#'   mandibular length is computed from the reflected point to `LM11`.
#' @param lm9_valid Logical. If `TRUE` (default), `LM9` is interpreted as a real
#'   anatomical gonion and measurements that depend on it are computed. If
#'   `FALSE`, `LM9` is treated as a placeholder used only to preserve the input
#'   structure, and measurements depending on `LM9` are returned as
#'   non-computable.
#' @param lm_coord_system Coordinate system of the numeric landmark values pasted
#'   into R. The default is `"LPS"`, matching the Avizo/Amira-like internal
#'   convention. Coordinates copied or exported from 3D Slicer Markups may paste
#'   as LPS even when the Slicer table displays R/A/S columns; in that case keep
#'   `lm_coord_system = "LPS"`. Use `"RAS"` only for values that are actually RAS,
#'   such as coordinates extracted from Slicer with
#'   `GetNthControlPointPositionWorld()`. This argument controls the spatial
#'   interpretation of the numbers only; it does not depend on whether the text
#'   was pasted as plain XYZ coordinates or as a Slicer Markups-style table.
#' @param SLICER Logical. If `FALSE` (default), generate Avizo/Amira TCL command
#'   blocks. If `TRUE`, generate 3D Slicer Python command blocks.
#' @param slicer_landmarks_str Deprecated alias for `landmarks_str`, retained so
#'   older scripts continue to run with the updated parser and coordinate logic.
#' @param landmark_coordinate_system Deprecated alias for `lm_coord_system`,
#'   retained for backward compatibility.
#' @param volume_name Optional scalar volume node name used by the generated
#'   Slicer Python block. If omitted, the block uses the active background volume
#'   in the chosen slice view when possible, falling back to the only scalar
#'   volume in the scene. If multiple scalar volumes are loaded, provide
#'   `volume_name` explicitly.
#'
#' @return An object of class `orientcsg_mandible` and
#'   `orientcsg_orientation`. The object is a list with the following
#'   components:
#'
#'   - `type`: Protocol type, here `"MANDIBLE"`.
#'   - `individual_id`: Specimen identifier.
#'   - `landmarks`: Landmark coordinate matrix.
#'   - `landmark_count`: Number of landmarks supplied.
#'   - `complete_arch`: Whether complete-arch mode was used.
#'   - `estimate_lm10`: Whether `LM10` was reflected for mandibular length.
#'   - `lm9_valid`: Whether `LM9` was treated as a real anatomical gonion.
#'   - `lm1_side`: Anatomical side on which `LM1` was placed.
#'   - `superoinferior_reference`: Reference point and vector used to orient
#'     `Vec_Penp` from inferior toward superior.
#'   - `points`: Computed points, including `LM1_Line`, `LM0`,
#'     `ARP_Origin`, `CS1B`, and `CS2B`; `LM9_Line` and `LM10_Line` are included
#'     when computed.
#'   - `vectors`: Computed orientation vectors.
#'   - `summary`: Compact summary table for key landmarks, computed points, and
#'     vectors used for plane orientation. Coordinates are emitted in the
#'     software-specific output coordinate system: LPS-like for Amira/Avizo when
#'     `SLICER = FALSE`, and RAS for 3D Slicer when `SLICER = TRUE`.
#'   - `summary_coord_system`: Coordinate system used by `summary`.
#'   - `output_coord_system`: Coordinate system used by software-specific outputs.
#'   - `measurements`: Linear mandibular measurements. The table includes
#'     `status` and `method` columns indicating whether each value is direct,
#'     estimated, computed, or non-computable.
#'   - `manual_orientation`: Table arranged for manual verification of ARP and
#'     section orientation in Avizo/Amira, when `SLICER = FALSE`.
#'   - `avizo_tcl`: Named list with TCL blocks for `CS1`, `CS2`, and `CS3`,
#'     when `SLICER = FALSE`.
#'   - `slicer_py`: Named list with 3D Slicer Python blocks for `CS1`, `CS2`,
#'     and `CS3`, when `SLICER = TRUE`. These blocks orient the Red slice view,
#'     create the ARP, `LM1_Line`, and scale-bar verification objects, configure
#'     volume rendering and a 3D verification view, and define `restore_view()`
#'     and `refresh_orientcsg_scale()`.
#'
#' @examples
#' \dontrun{
#' landmarks_str <- "
#' -30.80 -7.69 -143.70
#'  -1.58  7.37 -105.56
#'  -0.33 -22.72 -93.42
#'  -0.53 -16.99 -108.47
#' -28.76 -5.05 -132.70
#' -21.38 -4.79 -134.53
#' -25.12 -0.85 -121.62
#' -19.50 -0.58 -123.95
#' -46.74 -35.26 -164.05
#' -18.11  2.74 -110.29
#' -47.78 12.43 -201.18
#' "
#'
#' res <- orient_mandible(
#'   landmarks_str = landmarks_str,
#'   individual_id = "MANDIBLE_001",
#'   camera_distance_mm = 300,
#'   lm1_side = "RIGHT"
#' )
#'
#' res$summary
#' res$measurements
#' cat(get_tcl(res, section = "CS1"))
#'
#' res_slicer <- orient_mandible(
#'   landmarks_str = landmarks_str,
#'   individual_id = "MANDIBLE_001",
#'   lm_coord_system = "LPS",
#'   SLICER = TRUE,
#'   volume_name = "MANDIBLE_VOLUME"
#' )
#'
#' cat(get_slicer_py(res_slicer, section = "CS1"))
#' }
#'
#' @export
#'
orient_mandible <- function(landmarks_str = NULL,
                            individual_id = "MANDIBLE_001",
                            camera_distance_mm = 300,
                            lm1_side = c("RIGHT", "LEFT"),
                            complete_arch = FALSE,
                            estimate_lm10 = FALSE,
                            lm9_valid = TRUE,
                            lm_coord_system = "LPS",
                            SLICER = FALSE,
                            slicer_landmarks_str = NULL,
                            landmark_coordinate_system = NULL,
                            volume_name = NULL) {
  lm_coord_system_missing <- missing(lm_coord_system)
  lm1_side <- match.arg(toupper(lm1_side), c("RIGHT", "LEFT"))
  complete_arch <- assert_logical_scalar(complete_arch, "complete_arch")
  estimate_lm10 <- assert_logical_scalar(estimate_lm10, "estimate_lm10")
  lm9_valid <- assert_logical_scalar(lm9_valid, "lm9_valid")
  SLICER <- assert_logical_scalar(SLICER, "SLICER")
  lm_coord_system <- resolve_lm_coord_system(
    lm_coord_system = lm_coord_system,
    landmark_coordinate_system = landmark_coordinate_system,
    lm_coord_system_missing = lm_coord_system_missing
  )

  landmarks_str <- resolve_landmarks_str(
    landmarks_str = landmarks_str,
    slicer_landmarks_str = slicer_landmarks_str
  )

  mat_pts <- parse_mandible_landmarks(landmarks_str)
  mat_pts <- normalize_lm_coordinates(mat_pts, lm_coord_system = lm_coord_system)
  landmark_count <- nrow(mat_pts)

  if (estimate_lm10 && landmark_count < 11) {
    stop("estimate_lm10 = TRUE requires 11 or 12 mandibular landmarks, including LM10 and LM11.", call. = FALSE)
  }

  LM1 <- mat_pts["LM1", ]; LM2 <- mat_pts["LM2", ]; LM3 <- mat_pts["LM3", ]; LM4 <- mat_pts["LM4", ]
  LM5 <- mat_pts["LM5", ]; LM6 <- mat_pts["LM6", ]; LM7 <- mat_pts["LM7", ]; LM8 <- mat_pts["LM8", ]
  LM9 <- mat_pts["LM9", ]
  LM10 <- if ("LM10" %in% rownames(mat_pts)) mat_pts["LM10", ] else NULL
  LM11 <- if ("LM11" %in% rownames(mat_pts)) mat_pts["LM11", ] else NULL
  LM12 <- if ("LM12" %in% rownames(mat_pts)) mat_pts["LM12", ] else NULL

  # In the original fragmented workflow, LM1_Line is estimated by reflection
  # across plane P = LM2-LM3-LM4. In complete-arch mode, LM4 is treated as the
  # physically preserved LM1_Line/A_Line point.
  reflection_method <- NULL
  LM10_Line <- NULL

  if (complete_arch) {
    LM1_Line <- LM4
  } else {
    LM1_Line <- reflect_point_across_plane(LM1, LM2, LM3, LM4)
    reflection_method <- "plane_P_LM2_LM3_LM4"
  }

  Vec_1_1Line <- LM1_Line - LM1
  if (sqrt(sum(Vec_1_1Line^2)) < 1e-12) {
    stop("LM1 and LM1_Line coincide; Vec_1_1Line cannot be defined.", call. = FALSE)
  }

  # Build anatomical side vectors that do not depend on whether LM1 was placed
  # on the right or left mandibular side. Vec_RightToLeft always points from the
  # anatomical right side toward the anatomical left side. Vec_LandmarkedSide
  # points toward the side where LM1 was actually placed and is used to select
  # the viewing side for CS1 and CS2.
  Vec_RightToLeft <- if (identical(lm1_side, "RIGHT")) {
    nrm(Vec_1_1Line)
  } else {
    nrm(-Vec_1_1Line)
  }
  Vec_LeftToRight <- -Vec_RightToLeft
  Vec_LandmarkedSide <- if (identical(lm1_side, "RIGHT")) Vec_LeftToRight else Vec_RightToLeft

  # LM0 is the orthogonal projection of LM2 onto the LM1--LM1_Line axis. It is
  # used to define Vec_0_2, an anterior reference direction within the ARP.
  LM0 <- project_point_to_line(LM2, LM1, LM1_Line)
  Vec_0_2 <- LM2 - LM0
  if (sqrt(sum(Vec_0_2^2)) < 1e-12) {
    stop(
      "LM2 projects almost onto itself on the LM1--LM1_Line axis; Vec_0_2 cannot be defined.",
      call. = FALSE
    )
  }

  # Vec_Penp is the normal to the ARP. The name follows the original protocol,
  # where the vector was used to enforce perpendicularity of CS1 and CS2. Its
  # sign is oriented anatomically from inferior toward superior so that the
  # screen-vertical direction is consistent in both Avizo/Amira and Slicer
  # outputs.
  Vec_Penp <- nrm(cross3(Vec_0_2, Vec_1_1Line))
  si_reference <- select_mandible_si_reference(
    Vec_Penp = Vec_Penp,
    LM2 = LM2,
    LM3 = LM3,
    LM4 = LM4,
    LM9 = LM9,
    lm9_valid = lm9_valid,
    complete_arch = complete_arch
  )
  if (dot3(Vec_Penp, si_reference$reference_vector) < 0) {
    Vec_Penp <- -Vec_Penp
  }

  # The ARP is emitted to Amira/Avizo as origin + normal. The origin is the
  # centroid of the three points that conceptually define the ARP.
  ARP_Origin <- (LM1 + LM2 + LM1_Line) / 3

  if (complete_arch) {
    # Complete-arch symmetry plane: it passes through the midpoint of LM1 and
    # the real LM1_Line/A_Line point, contains the anterior direction toward LM2,
    # and is perpendicular to the ARP. This provides the reflection plane used for
    # contralateral estimates when the dental arch is preserved.
    M_1_1Line <- (LM1 + LM1_Line) / 2
    reflection_A <- M_1_1Line
    reflection_B <- LM2
    reflection_C <- M_1_1Line + Vec_Penp
    reflection_method <- "complete_arch_plane_LM1_LM4_LM2_ARP"
  } else {
    reflection_A <- LM2
    reflection_B <- LM3
    reflection_C <- LM4
  }

  LM9_Line <- NULL
  if (lm9_valid) {
    LM9_Line <- reflect_point_across_plane(LM9, reflection_A, reflection_B, reflection_C)
  }
  if (estimate_lm10) {
    LM10_Line <- reflect_point_across_plane(LM10, reflection_A, reflection_B, reflection_C)
  }

  CS1B <- LM5
  CS2B <- LM7
  Vec_CS1 <- LM6 - LM5
  Vec_CS2 <- LM8 - LM7

  if (sqrt(sum(Vec_CS1^2)) < 1e-12) {
    stop("LM5 and LM6 coincide; Vec_CS1 cannot be defined.", call. = FALSE)
  }
  if (sqrt(sum(Vec_CS2^2)) < 1e-12) {
    stop("LM7 and LM8 coincide; Vec_CS2 cannot be defined.", call. = FALSE)
  }

  Vec_CS1_Normal <- nrm(cross3(Vec_CS1, Vec_Penp))
  Vec_CS2_Normal <- nrm(cross3(Vec_CS2, Vec_Penp))

  Anterior_ref <- nrm(Vec_0_2)

  summary_entries <- list(
    LM1 = LM1,
    LM2 = LM2,
    LM3 = LM3
  )

  if (!complete_arch) {
    summary_entries$LM4 <- LM4
  }

  summary_entries <- c(
    summary_entries,
    list(
      LM1_Line = LM1_Line,
      ARP_Origin = ARP_Origin,
      Vec_Penp = Vec_Penp,
      CS1B = CS1B,
      Vec_CS1_Normal = Vec_CS1_Normal,
      CS2B = CS2B,
      Vec_CS2_Normal = Vec_CS2_Normal,
      Vec_1_1Line = Vec_1_1Line,
      LM0 = LM0,
      LM6 = LM6,
      LM8 = LM8,
      LM9 = LM9
    )
  )
  if (!is.null(LM10)) summary_entries$LM10 <- LM10
  if (!is.null(LM11)) summary_entries$LM11 <- LM11
  if (!is.null(LM12)) summary_entries$LM12 <- LM12
  if (!is.null(LM10_Line)) summary_entries$LM10_Line <- LM10_Line

  summary_tbl <- make_xyz_summary(summary_entries, individual_id)

  summary_coord_system <- if (isTRUE(SLICER)) "RAS" else "LPS"
  if (isTRUE(SLICER)) {
    summary_coords <- as.matrix(summary_tbl[, c("x", "y", "z"), drop = FALSE])
    summary_coords <- flip_xy_matrix(summary_coords)
    summary_tbl[, c("x", "y", "z")] <- round(summary_coords, 6)
  }
  attr(summary_tbl, "coord_system") <- summary_coord_system

  measurements <- build_mandible_measurements(
    individual_id = individual_id,
    LM1 = LM1,
    LM2 = LM2,
    LM3 = LM3,
    LM4 = LM4,
    LM9 = LM9,
    LM10 = LM10,
    LM11 = LM11,
    LM12 = LM12,
    LM0 = LM0,
    LM1_Line = LM1_Line,
    LM9_Line = LM9_Line,
    LM10_Line = LM10_Line,
    complete_arch = complete_arch,
    estimate_lm10 = estimate_lm10,
    lm9_valid = lm9_valid,
    reflection_method = reflection_method
  )

  if (!isTRUE(SLICER)) {
    # This table mirrors the logic of the protocol and is meant for checking or
    # reproducing the orientation manually in Avizo if needed.
    manual_orientation <- rbind(
      data.frame(section = "ARP", role = "Origin", object = "ARP", value = "ARP_Origin", x = ARP_Origin[1], y = ARP_Origin[2], z = ARP_Origin[3]),
      data.frame(section = "ARP", role = "Normal", object = "ARP", value = "Vec_Penp", x = Vec_Penp[1], y = Vec_Penp[2], z = Vec_Penp[3]),
      data.frame(section = "CS1", role = "Plane point", object = "Slice", value = "CS1B/LM5", x = CS1B[1], y = CS1B[2], z = CS1B[3]),
      data.frame(section = "CS1", role = "Normal", object = "Slice", value = "Vec_CS1_Normal", x = Vec_CS1_Normal[1], y = Vec_CS1_Normal[2], z = Vec_CS1_Normal[3]),
      data.frame(section = "CS1", role = "Screen-horizontal reference", object = "Camera", value = "Vec_CS1", x = Vec_CS1[1], y = Vec_CS1[2], z = Vec_CS1[3]),
      data.frame(section = "CS2", role = "Plane point", object = "Slice", value = "CS2B/LM7", x = CS2B[1], y = CS2B[2], z = CS2B[3]),
      data.frame(section = "CS2", role = "Normal", object = "Slice", value = "Vec_CS2_Normal", x = Vec_CS2_Normal[1], y = Vec_CS2_Normal[2], z = Vec_CS2_Normal[3]),
      data.frame(section = "CS2", role = "Screen-horizontal reference", object = "Camera", value = "Vec_CS2", x = Vec_CS2[1], y = Vec_CS2[2], z = Vec_CS2[3]),
      data.frame(section = "CS3", role = "Plane point", object = "Slice", value = "LM2", x = LM2[1], y = LM2[2], z = LM2[3]),
      data.frame(section = "CS3", role = "Normal", object = "Slice", value = "Vec_RightToLeft", x = Vec_RightToLeft[1], y = Vec_RightToLeft[2], z = Vec_RightToLeft[3]),
      data.frame(section = "CS3", role = "Screen-horizontal reference", object = "Camera", value = "Vec_0_2", x = Vec_0_2[1], y = Vec_0_2[2], z = Vec_0_2[3])
    )
    numeric_cols <- vapply(manual_orientation, is.numeric, logical(1))
    manual_orientation[numeric_cols] <- lapply(manual_orientation[numeric_cols], round, 6)

  }

  vectors <- list(
    Vec_1_1Line = Vec_1_1Line,
    Vec_0_2 = Vec_0_2,
    Vec_Penp = Vec_Penp,
    Vec_CS1 = Vec_CS1,
    Vec_CS1_Normal = Vec_CS1_Normal,
    Vec_CS2 = Vec_CS2,
    Vec_CS2_Normal = Vec_CS2_Normal,
    Anterior_ref = Anterior_ref,
    Vec_RightToLeft = Vec_RightToLeft,
    Vec_LeftToRight = Vec_LeftToRight,
    Vec_LandmarkedSide = Vec_LandmarkedSide,
    Superoinferior_ref = si_reference$reference_vector
  )
  points <- list(
    LM1_Line = LM1_Line,
    LM0 = LM0,
    ARP_Origin = ARP_Origin,
    CS1B = CS1B,
    CS2B = CS2B
  )
  if (!is.null(LM9_Line)) points$LM9_Line <- LM9_Line
  if (!is.null(LM10_Line)) points$LM10_Line <- LM10_Line

  res <- list(
    type = "MANDIBLE",
    individual_id = individual_id,
    landmarks = mat_pts,
    landmark_count = landmark_count,
    complete_arch = complete_arch,
    estimate_lm10 = estimate_lm10,
    lm9_valid = lm9_valid,
    reflection_method = reflection_method,
    superoinferior_reference = si_reference,
    points = points,
    vectors = vectors,
    summary = summary_tbl,
    summary_coord_system = summary_coord_system,
    measurements = measurements,
    camera_distance_mm = camera_distance_mm,
    lm1_side = lm1_side,
    lm_coord_system = lm_coord_system,
    internal_coord_system = "LPS",
    output_coord_system = summary_coord_system,
    SLICER = SLICER,
    volume_name = volume_name
  )
  if (!isTRUE(SLICER)) {
    res$manual_orientation <- manual_orientation
  }

  class(res) <- c("orientcsg_mandible", "orientcsg_orientation")

  if (isTRUE(SLICER)) {
    res$slicer_py <- lapply(c("CS1", "CS2", "CS3"), function(sec) {
      emit_slicer_mandible_python(res, section = sec)
    })
    names(res$slicer_py) <- c("CS1", "CS2", "CS3")
  } else {
    res$avizo_tcl <- avizo_tcl_mandible(res)
  }

  res
}

# Internal mandibular parsing helper -----------------------------------------
#
# Accept exactly the preservation patterns supported by the mandibular workflow:
# 9 landmarks (LM1-LM9), 11 landmarks (LM1-LM11), or 12 landmarks (LM1-LM12).
parse_mandible_landmarks <- function(landmarks_str) {
  allowed_landmarks <- c(9L, 11L, 12L)
  lines <- clean_landmark_lines(landmarks_str)
  token_list <- lapply(lines, extract_numeric_tokens)
  token_lengths <- vapply(token_list, length, integer(1))

  linewise_n <- if (length(lines) %in% allowed_landmarks &&
                    all(token_lengths >= 3L) &&
                    (all(token_lengths == 3L) || all(token_lengths >= 4L))) {
    length(lines)
  } else {
    NA_integer_
  }

  if (!is.na(linewise_n)) {
    out <- parse_landmarks(landmarks_str, n_landmarks = linewise_n, context = "MANDIBLE")
  } else {
    nums <- extract_numeric_tokens(landmarks_str)
    allowed_values <- allowed_landmarks * 3L

    if (!(length(nums) %in% allowed_values)) {
      stop(
        sprintf(
          "MANDIBLE requires 27, 33, or 36 numeric values: 9, 11, or 12 landmarks x 3 coordinates. Detected %d numeric values.",
          length(nums)
        ),
        call. = FALSE
      )
    }

    out <- matrix(nums, ncol = 3, byrow = TRUE)
  }

  rownames(out) <- paste0("LM", seq_len(nrow(out)))
  colnames(out) <- c("x", "y", "z")
  out
}

# Internal scalar validation helper ------------------------------------------
assert_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("%s must be TRUE or FALSE.", arg), call. = FALSE)
  }
  x
}

# Internal mandibular orientation helper --------------------------------------
#
# Select the anatomical reference used to choose the sign of Vec_Penp. The
# returned vector points from an inferior landmark or placeholder toward LM2, so
# Vec_Penp can be forced to point inferior-to-superior. This sign is then used
# consistently by both the Avizo/Amira camera backend and the 3D Slicer backend.
select_mandible_si_reference <- function(Vec_Penp, LM2, LM3, LM4, LM9,
                                         lm9_valid, complete_arch) {
  candidate <- NULL
  source <- NULL

  if (isTRUE(lm9_valid)) {
    candidate <- LM9
    source <- "LM9_real_gonion"
  } else {
    # In standard incomplete-arch workflows LM3 and LM4 are part of the
    # inferior reference geometry used to define the reflection plane. In
    # complete-arch workflows LM4 is the real LM1_Line/A_Line point, so LM3 is
    # preferred as the non-gonial inferior reference.
    candidates <- if (isTRUE(complete_arch)) {
      list(LM3 = LM3)
    } else {
      list(LM3 = LM3, LM4 = LM4)
    }

    offsets <- vapply(
      candidates,
      function(p) abs(dot3(Vec_Penp, LM2 - p)),
      numeric(1)
    )

    if (length(offsets) > 0 && max(offsets, na.rm = TRUE) > 1e-8) {
      idx <- which.max(offsets)
      candidate <- candidates[[idx]]
      source <- paste0(names(candidates)[idx], "_inferior_reference")
    } else {
      candidate <- LM9
      source <- "LM9_orientation_only"
    }
  }

  ref <- LM2 - candidate
  if (sqrt(sum(ref^2)) < 1e-12) {
    stop(
      "The selected mandibular superoinferior reference coincides with LM2; Vec_Penp sign cannot be defined.",
      call. = FALSE
    )
  }

  list(
    source = source,
    inferior_point = candidate,
    superior_point = LM2,
    reference_vector = nrm(ref)
  )
}

# Internal summary helper -----------------------------------------------------
make_xyz_summary <- function(entries, individual_id) {
  mat <- do.call(rbind, entries)
  data.frame(
    Individual = rep(individual_id, length(entries)),
    metric = names(entries),
    x = round(mat[, 1], 6),
    y = round(mat[, 2], 6),
    z = round(mat[, 3], 6),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Internal measurement helper -------------------------------------------------
build_mandible_measurements <- function(individual_id,
                                        LM1,
                                        LM2,
                                        LM3,
                                        LM4,
                                        LM9,
                                        LM10,
                                        LM11,
                                        LM12,
                                        LM0,
                                        LM1_Line,
                                        LM9_Line,
                                        LM10_Line,
                                        complete_arch,
                                        estimate_lm10,
                                        lm9_valid,
                                        reflection_method) {
  rows <- list()

  add_measurement <- function(metric, value_mm, status, method) {
    data.frame(
      Individual = individual_id,
      metric = metric,
      value_mm = if (is.na(value_mm)) NA_real_ else round(value_mm, 6),
      status = status,
      method = method,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (lm9_valid) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Corpus_length",
      dist3(LM3, LM9),
      "direct",
      "LM3_LM9"
    )
  } else {
    rows[[length(rows) + 1]] <- add_measurement(
      "Corpus_length",
      NA_real_,
      "uncomputable",
      "missing_or_invalid_LM9"
    )
  }

  if (is.null(LM10) || is.null(LM11)) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Mandibular_length",
      NA_real_,
      "uncomputable",
      "missing_LM10_LM11"
    )
  } else if (estimate_lm10) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Mandibular_length",
      dist3(LM10_Line, LM11),
      "estimated",
      paste0("reflected_LM10_", reflection_method, "_to_LM11")
    )
  } else {
    rows[[length(rows) + 1]] <- add_measurement(
      "Mandibular_length",
      dist3(LM10, LM11),
      "direct",
      "LM10_LM11"
    )
  }

  if (complete_arch) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Dental_arch_breadth",
      dist3(LM1, LM4),
      "direct",
      "LM1_LM4_A_Line"
    )
  } else {
    rows[[length(rows) + 1]] <- add_measurement(
      "Dental_arch_breadth",
      dist3(LM1, LM1_Line),
      "estimated",
      paste0("reflected_LM1_", reflection_method)
    )
  }

  rows[[length(rows) + 1]] <- add_measurement(
    "Dental_arch_superior_length",
    dist3(LM2, LM0),
    "computed",
    "LM2_LM0_projection_on_LM1_LM1_Line"
  )

  if (!lm9_valid) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Bigonial_breadth",
      NA_real_,
      "uncomputable",
      "missing_or_invalid_LM9"
    )
  } else if (!is.null(LM12)) {
    rows[[length(rows) + 1]] <- add_measurement(
      "Bigonial_breadth",
      dist3(LM9, LM12),
      "direct",
      "LM9_LM12"
    )
  } else {
    rows[[length(rows) + 1]] <- add_measurement(
      "Bigonial_breadth",
      dist3(LM9, LM9_Line),
      "estimated",
      paste0("reflected_LM9_", reflection_method)
    )
  }

  do.call(rbind, rows)
}

# Internal mandibular TCL generator -----------------------------------------
#
# Transform the mandibular orientation object into three Avizo TCL blocks, one
# for each planned cross-section. The geometric calculations are completed before
# this function is called; here the task is only to translate those results into
# Avizo command syntax.
avizo_tcl_mandible <- function(res) {
  LM <- res$landmarks
  LM1 <- LM["LM1", ]
  LM2 <- LM["LM2", ]
  LM1_Line <- res$points$LM1_Line
  Vec_Penp <- res$vectors$Vec_Penp
  Vec_0_2 <- res$vectors$Vec_0_2
  Vec_1_1Line <- res$vectors$Vec_1_1Line
  Anterior_ref <- res$vectors$Anterior_ref
  Vec_LandmarkedSide <- res$vectors$Vec_LandmarkedSide
  Vec_RightToLeft <- res$vectors$Vec_RightToLeft
  camdist <- res$camera_distance_mm

  arp_block <- function() {
    emit_plane_3points("ARP", LM1, LM2, LM1_Line, color = c(0, 1, 0), hide_points = TRUE)
  }

  cs12_block <- function(section_label, Psec, Vec_CS) {
    X_screen <- project_vector_to_plane(Vec_CS, Vec_Penp)
    if (sqrt(sum(X_screen^2)) < 1e-12) {
      stop(sprintf("%s: Vec_CS is collinear with Vec_Penp.", section_label), call. = FALSE)
    }
    X_screen <- nrm(X_screen)

    # CS1 and CS2 are geometrically defined by the landmark section direction
    # and the ARP normal. For better Amira/Avizo compatibility, the Slice object
    # is emitted as a normal-and-point plane. This is equivalent to the previous
    # point-and-two-vectors definition because the slice normal is perpendicular
    # to both in-plane vectors.
    Z_slice <- nrm(cross3(Vec_CS, Vec_Penp))

    # The camera direction is perpendicular to both the screen-horizontal section
    # vector and the ARP normal. Its sign is selected so that CS1 and CS2 are
    # viewed from the side on which LM1 was placed.
    Z_camera <- nrm(cross3(X_screen, Vec_Penp))
    if (dot3(Z_camera, Vec_LandmarkedSide) < 0) Z_camera <- -Z_camera

    c(
      "# ============================================================",
      sprintf("# MANDIBLE - %s", section_label),
      "# ============================================================",
      "",
      "# ARP clipping plane: 3 points = LM1, LM2, LM1_Line",
      paste(arp_block(), collapse = "\n"),
      "",
      sprintf("# Slice object: normal & point for %s", section_label),
      paste(emit_slice_normal_point("Slice", Psec, Z_slice), collapse = "\n"),
      "",
      paste(emit_optional_orthogonal_view(Psec, X_screen, section_label), collapse = "\n"),
      "",
      "# Camera: slice parallel to screen; ARP horizontal; view from LM1 side",
      paste(emit_camera_from_basis(Psec, Z_camera, X_screen, Y_preferred = Vec_Penp, camDist = camdist), collapse = "\n")
    )
  }

  cs3_block <- function() {
    Psec <- LM2
    Z_camera <- nrm(Vec_LandmarkedSide)
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
      paste(emit_slice_normal_point("Slice", Psec, Vec_RightToLeft), collapse = "\n"),
      "",
      paste(emit_optional_orthogonal_view(Psec, X_screen, "CS3"), collapse = "\n"),
      "",
      sprintf("# Camera: slice parallel to screen; ARP horizontal; LM1 side = %s", res$lm1_side),
      paste(emit_camera_from_basis(Psec, Z_camera, X_screen, Y_preferred = Vec_Penp, camDist = camdist), collapse = "\n")
    )
  }

  list(
    CS1 = paste(cs12_block("CS1", res$points$CS1B, res$vectors$Vec_CS1), collapse = "\n"),
    CS2 = paste(cs12_block("CS2", res$points$CS2B, res$vectors$Vec_CS2), collapse = "\n"),
    CS3 = paste(cs3_block(), collapse = "\n")
  )
}
