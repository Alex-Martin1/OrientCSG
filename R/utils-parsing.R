# Internal parsing helper ----------------------------------------------------
#
# Extract numeric values from a free-form text block. This allows the package to
# accept coordinates copied directly from Avizo/Amira, BoneJ logs, or plain text
# files, even when values are separated by spaces, tabs, commas, semicolons, or
# vertical bars.
extract_nums <- function(txt) {
  clean <- gsub("[|,;]", " ", txt)
  scan(text = clean, quiet = TRUE)
}

# Internal parsing helper ----------------------------------------------------
#
# Extract numeric tokens from text that may also contain labels such as F-1.
# This is used for coordinate tables copied from 3D Slicer Markups, where labels
# and IDs are mixed with coordinate values. The parser reads the numeric text only;
# the spatial coordinate system is declared separately with lm_coord_system.
extract_numeric_tokens <- function(txt) {
  pattern <- "[-+]?(?:\\d*\\.\\d+|\\d+\\.?\\d*)(?:[eE][-+]?\\d+)?"
  matches <- gregexpr(pattern, txt, perl = TRUE)
  out <- regmatches(txt, matches)[[1]]
  as.numeric(out)
}

# Internal parsing helper ----------------------------------------------------
#
# Keep only non-empty, non-comment lines from a landmark text block.
clean_landmark_lines <- function(txt) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]
  lines
}

# Internal coordinate helper -------------------------------------------------
#
# Convert between RAS and LPS/external conventions by inverting X and Y. The
# operation is its own inverse, so the same helper is used in both directions.
flip_xy <- function(x) {
  x <- as.numeric(x)
  c(-x[1], -x[2], x[3])
}

# Internal coordinate helper -------------------------------------------------
#
# Apply flip_xy() row-wise to a coordinate matrix while preserving names.
flip_xy_matrix <- function(mat) {
  out <- as.matrix(mat)
  out[, 1] <- -out[, 1]
  out[, 2] <- -out[, 2]
  colnames(out) <- colnames(mat)
  rownames(out) <- rownames(mat)
  out
}

# Internal argument helper ---------------------------------------------------
#
# Validate the coordinate-system argument used to interpret landmark values.
resolve_lm_coord_system <- function(lm_coord_system = "LPS") {
  lm_coord_system <- toupper(trimws(lm_coord_system))
  if (length(lm_coord_system) != 1L || is.na(lm_coord_system) || !lm_coord_system %in% c("LPS", "RAS")) {
    stop('`lm_coord_system` must be "LPS" or "RAS".', call. = FALSE)
  }

  lm_coord_system
}

# Internal argument helper ---------------------------------------------------
#
# Validate the landmark text argument.
resolve_landmarks_str <- function(landmarks_str = NULL) {
  if (is.null(landmarks_str) || length(landmarks_str) != 1L || !nzchar(trimws(landmarks_str))) {
    stop("`landmarks_str` is required.", call. = FALSE)
  }

  landmarks_str
}

# Internal coordinate helper -------------------------------------------------
#
# Convert input landmark coordinates to the package's internal LPS/external
# convention. The textual input format is deliberately handled elsewhere: this
# function only interprets the coordinate system of the numeric values that were
# actually pasted into R. Coordinates copied/exported from 3D Slicer Markups may
# paste as LPS even when the Slicer table displays R/A/S columns.
normalize_lm_coordinates <- function(coords, lm_coord_system = "LPS", arg = "lm_coord_system") {
  lm_coord_system <- toupper(trimws(lm_coord_system))

  if (length(lm_coord_system) != 1L || is.na(lm_coord_system) || !lm_coord_system %in% c("LPS", "RAS")) {
    stop(sprintf('`%s` must be "LPS" or "RAS".', arg), call. = FALSE)
  }

  out <- as.matrix(coords)
  if (ncol(out) != 3L) {
    stop("Landmark coordinates must be a matrix with three columns.", call. = FALSE)
  }

  if (lm_coord_system == "RAS") {
    out <- flip_xy_matrix(out)
  }

  colnames(out) <- c("x", "y", "z")
  rownames(out) <- rownames(coords)
  out
}

# Internal parsing helper ----------------------------------------------------
#
# Convert a landmark coordinate string into an n x 3 numeric matrix. The parser
# accepts two common textual formats without using that format to infer the
# coordinate system:
#   1. Plain XYZ coordinates, one landmark per line or as a numeric stream.
#   2. Slicer-style rows, where columns 2:4 contain X, Y and Z coordinates.
#
# For tibial Slicer-style rows, the historical OrientCSG order is preserved:
# row 1 = Plateau2, row 2 = Plateau1, row 3 = TibioTalar. The returned matrix is
# reordered to the internal order Plateau1, Plateau2, TibioTalar.
parse_landmarks <- function(landmarks_str, n_landmarks, context = "landmarks") {
  context_upper <- toupper(trimws(context))
  expected_n <- n_landmarks * 3

  lines <- clean_landmark_lines(landmarks_str)
  token_list <- lapply(lines, extract_numeric_tokens)
  token_lengths <- vapply(token_list, length, integer(1))

  detected_table <- length(lines) >= n_landmarks &&
    all(token_lengths[seq_len(n_landmarks)] >= 4L) &&
    any(token_lengths[seq_len(n_landmarks)] > 3L)

  detected_plain_lines <- length(lines) == n_landmarks &&
    all(token_lengths == 3L)

  if (detected_table) {
    if (length(lines) > n_landmarks) {
      warning(
        sprintf(
          "%d landmark rows were detected; only the first %d will be used.",
          length(lines), n_landmarks
        ),
        call. = FALSE
      )
    }

    mat <- do.call(
      rbind,
      lapply(token_list[seq_len(n_landmarks)], function(x) x[2:4])
    )
  } else if (detected_plain_lines) {
    mat <- do.call(rbind, token_list)
  } else {
    nums <- extract_numeric_tokens(landmarks_str)

    if (length(nums) < expected_n) {
      stop(
        sprintf(
          "%s requires %d numeric values: %d landmarks x 3 coordinates.",
          context, expected_n, n_landmarks
        ),
        call. = FALSE
      )
    }

    if (length(nums) > expected_n) {
      warning(
        sprintf(
          "%d numeric values were detected; only the first %d will be used.",
          length(nums), expected_n
        ),
        call. = FALSE
      )
    }

    mat <- matrix(nums[seq_len(expected_n)], ncol = 3, byrow = TRUE)
  }

  colnames(mat) <- c("x", "y", "z")

  if (detected_table && identical(context_upper, "TIBIA") && nrow(mat) == 3L) {
    mat <- mat[c(2, 1, 3), , drop = FALSE]
  }

  mat
}

# Internal parsing helper ----------------------------------------------------
#
# Read BoneJ Moments of Inertia eigenvectors. Three input formats are accepted:
#   1. A direct three-component longitudinal vector (x, y, z).
#   2. The legacy 3 x 3 eigenvector matrix copied from the Log window.
#   3. A full BoneJ Results-table row containing the unit-vector columns.
#
# For the Results-table row format, the final nine numeric fields are interpreted
# as vector0{x, y, z}, vector1{x, y, z}, and vector2{x, y, z}. The orientation
# workflow uses the direct vector or the first matrix/table vector as the
# longitudinal direction, following the established long-bone protocol. Numeric
# fields are parsed token by token rather than with a broad regex so that specimen
# IDs such as AAM_T-181_tibia are not misread as measurements.
parse_bonej_eigenvectors <- function(longitudinal_matrix_str) {
  if (is.null(longitudinal_matrix_str) ||
      length(longitudinal_matrix_str) != 1L ||
      !nzchar(trimws(longitudinal_matrix_str))) {
    stop("`longitudinal_matrix_str` is required.", call. = FALSE)
  }

  clean <- gsub("[|,;]", " ", longitudinal_matrix_str)
  fields <- unlist(strsplit(trimws(clean), "\\s+", perl = TRUE), use.names = FALSE)
  nums <- suppressWarnings(as.numeric(fields))
  nums <- nums[is.finite(nums)]

  if (length(nums) == 3L) {
    return(matrix(nums, nrow = 3L, ncol = 1L))
  }

  if (length(nums) == 9L) {
    return(matrix(nums, nrow = 3L, byrow = TRUE))
  }

  if (length(nums) > 9L) {
    eig <- utils::tail(nums, 9L)
    return(cbind(
      eig[1:3],
      eig[4:6],
      eig[7:9]
    ))
  }

  stop(
    paste0(
      "`longitudinal_matrix_str` must contain either 3 numeric values ",
      "defining the BoneJ longitudinal vector, 9 numeric values defining ",
      "the legacy 3 x 3 eigenvector matrix, or a full BoneJ Results-table ",
      "row containing the final nine unit-vector values."
    ),
    call. = FALSE
  )
}

# Internal DICOM helpers -----------------------------------------------------
#
# Parse DICOM Image Orientation (Patient) (0020,0037). The input may be a
# numeric vector with six values or the complete line copied from the exact
# DICOM stack used in BoneJ. When a complete line is supplied, the final six
# numeric values are interpreted as the two in-plane direction-cosine triplets.
parse_dicom_iop <- function(dicom_iop) {
  if (is.numeric(dicom_iop)) {
    if (length(dicom_iop) != 6L || any(!is.finite(dicom_iop))) {
      stop("`dicom_iop` must contain six finite numeric values.", call. = FALSE)
    }
    return(as.numeric(dicom_iop))
  }

  if (!is.character(dicom_iop) || length(dicom_iop) != 1L || !nzchar(trimws(dicom_iop))) {
    stop(
      "`dicom_iop` must be a numeric vector of length 6 or a pasted DICOM Image Orientation (Patient) line.",
      call. = FALSE
    )
  }

  nums <- extract_numeric_tokens(dicom_iop)
  if (length(nums) >= 6L) nums <- utils::tail(nums, 6L)

  if (length(nums) != 6L || any(!is.finite(nums))) {
    stop("Could not parse six finite values from `dicom_iop`.", call. = FALSE)
  }

  as.numeric(nums)
}

# Parse DICOM Image Position (Patient) (0020,0032). The input may be a numeric
# XYZ triplet or the complete DICOM line. For TRUE-volume workflows,
# `dicom_ipp_1` and `dicom_ipp_2` must describe two consecutive slices in the
# same order in which those slices occur in the ImageJ/BoneJ stack.
parse_dicom_ipp <- function(dicom_ipp, arg_name = "dicom_ipp") {
  if (is.numeric(dicom_ipp)) {
    if (length(dicom_ipp) != 3L || any(!is.finite(dicom_ipp))) {
      stop(sprintf("`%s` must contain three finite numeric values.", arg_name), call. = FALSE)
    }
    return(as.numeric(dicom_ipp))
  }

  if (!is.character(dicom_ipp) || length(dicom_ipp) != 1L || !nzchar(trimws(dicom_ipp))) {
    stop(
      sprintf("`%s` must be a numeric vector of length 3 or a pasted DICOM Image Position (Patient) line.", arg_name),
      call. = FALSE
    )
  }

  nums <- extract_numeric_tokens(dicom_ipp)
  if (length(nums) >= 3L) nums <- utils::tail(nums, 3L)

  if (length(nums) != 3L || any(!is.finite(nums))) {
    stop(sprintf("Could not parse three finite values from `%s`.", arg_name), call. = FALSE)
  }

  as.numeric(nums)
}

# Format parsed DICOM values compactly for the result summary while retaining
# sufficient numeric precision for traceability.
format_dicom_values <- function(x) {
  paste(vapply(as.numeric(x), function(v) {
    format(v, digits = 15, scientific = FALSE, trim = TRUE)
  }, character(1)), collapse = "\\")
}

# Build the transformation from the ImageJ/BoneJ stack basis to the internal
# DICOM/LPS patient basis. IOP defines the two in-plane stack axes. Their cross
# product defines the slice normal up to sign; two consecutive IPP positions,
# supplied in BoneJ stack order, determine that sign. This avoids assuming that
# stack index Z necessarily increases in the IOP cross-product direction.
dicom_geometry_to_bonej_transform <- function(dicom_iop,
                                               dicom_ipp_1,
                                               dicom_ipp_2,
                                               alignment_tolerance = 0.999) {
  iop <- parse_dicom_iop(dicom_iop)
  ipp_1 <- parse_dicom_ipp(dicom_ipp_1, "dicom_ipp_1")
  ipp_2 <- parse_dicom_ipp(dicom_ipp_2, "dicom_ipp_2")

  row_axis <- nrm(iop[1:3])
  col_axis <- nrm(iop[4:6])

  dot_rc <- dot3(row_axis, col_axis)
  if (abs(dot_rc) > 1e-4) {
    warning(
      "The DICOM row and column direction cosines are not orthogonal; the column axis was re-orthogonalized.",
      call. = FALSE
    )
  }

  col_axis <- col_axis - dot3(col_axis, row_axis) * row_axis
  if (sqrt(sum(col_axis^2)) < 1e-12) {
    stop("The DICOM row and column direction cosines are collinear.", call. = FALSE)
  }
  col_axis <- nrm(col_axis)

  iop_normal <- nrm(cross3(row_axis, col_axis))
  ipp_delta <- ipp_2 - ipp_1
  slice_spacing <- sqrt(sum(ipp_delta^2))
  if (!is.finite(slice_spacing) || slice_spacing < 1e-12) {
    stop(
      "`dicom_ipp_1` and `dicom_ipp_2` must refer to two distinct consecutive slices in BoneJ stack order.",
      call. = FALSE
    )
  }

  ipp_direction <- ipp_delta / slice_spacing
  signed_alignment <- dot3(ipp_direction, iop_normal)
  alignment <- abs(signed_alignment)
  if (!is.finite(alignment) || alignment < alignment_tolerance) {
    stop(
      paste0(
        "The displacement from `dicom_ipp_1` to `dicom_ipp_2` is not parallel to the DICOM image-plane normal. ",
        "Use Image Position (Patient) values from two consecutive slices, in the exact order used in the BoneJ stack, ",
        "and do not use a stack that was reoriented or resliced after DICOM import."
      ),
      call. = FALSE
    )
  }

  slice_direction <- if (signed_alignment < 0) -1 else 1
  slice_axis <- slice_direction * iop_normal

  out <- cbind(row_axis, col_axis, slice_axis)
  rownames(out) <- c("x", "y", "z")
  colnames(out) <- c("stack_x", "stack_y", "stack_z")

  attr(out, "dicom_iop") <- iop
  attr(out, "dicom_ipp_1") <- ipp_1
  attr(out, "dicom_ipp_2") <- ipp_2
  attr(out, "iop_normal") <- iop_normal
  attr(out, "slice_direction") <- slice_direction
  attr(out, "slice_spacing") <- slice_spacing
  attr(out, "slice_alignment") <- alignment
  out
}
