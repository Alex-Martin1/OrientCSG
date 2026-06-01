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
# Read the 3 x 3 eigenvector matrix returned by BoneJ's Moments of Inertia
# module. The orientation workflow uses the first column as the longitudinal
# direction, following the established long-bone protocol.
parse_bonej_eigenvectors <- function(longitudinal_matrix_str) {
  nums <- extract_nums(longitudinal_matrix_str)

  if (length(nums) < 9) {
    stop("The longitudinal matrix must contain at least 9 numeric values (3 x 3).", call. = FALSE)
  }

  matrix(nums[1:9], nrow = 3, byrow = TRUE)
}

# Internal DICOM helper ------------------------------------------------------
#
# Parse the DICOM Image Orientation (Patient) field (0020,0037). The input can
# be either a numeric vector with six values or a pasted DICOM line such as
# "0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0". If a full
# DICOM line is supplied, the tag numbers are ignored and the last six numeric
# values are interpreted as the row and column direction cosines.
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
  if (length(nums) >= 6L) {
    nums <- utils::tail(nums, 6L)
  }

  if (length(nums) != 6L || any(!is.finite(nums))) {
    stop("Could not parse six finite values from `dicom_iop`.", call. = FALSE)
  }

  as.numeric(nums)
}

# Internal DICOM helper ------------------------------------------------------
#
# Build the vector transformation from the ImageJ/BoneJ stack basis to the
# DICOM patient basis used internally by the classic Avizo/Amira workflow. The
# first IOP triplet gives the direction of the image-row axis, the second gives
# the image-column axis, and their cross product gives the slice-normal axis.
dicom_iop_to_bonej_transform <- function(dicom_iop) {
  iop <- parse_dicom_iop(dicom_iop)

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
  slice_axis <- nrm(cross3(row_axis, col_axis))

  out <- cbind(row_axis, col_axis, slice_axis)
  rownames(out) <- c("x", "y", "z")
  colnames(out) <- c("stack_x", "stack_y", "stack_z")
  attr(out, "dicom_iop") <- iop
  out
}

# Internal argument helper ---------------------------------------------------
#
# Resolve how the BoneJ eigenvector matrix is transferred into the package's
# internal LPS/DICOM coordinate convention. The default uses DICOM Image
# Orientation (Patient). `flip_xy` reproduces workflows developed before this
# DICOM-aware conversion was added.
resolve_bonej_transform <- function(bonej_coord_transform = "dicom_iop",
                                    dicom_iop = NULL,
                                    bonej_transform_matrix = NULL) {
  if (is.null(bonej_coord_transform) || length(bonej_coord_transform) != 1L || is.na(bonej_coord_transform)) {
    stop("`bonej_coord_transform` must be a single character value.", call. = FALSE)
  }

  transform_name <- tolower(trimws(bonej_coord_transform))

  if (identical(transform_name, "dicom_iop")) {
    if (is.null(dicom_iop)) {
      stop(
        paste0(
          "`dicom_iop` is required when `SOLID = FALSE` and ",
          "`bonej_coord_transform = 'dicom_iop'`. Paste the DICOM ",
          "Image Orientation (Patient) line, for example: ",
          "dicom_iop = r\"(0020,0037 Image Orientation (Patient): -1\\0\\0\\0\\-1\\0)\"."
        ),
        call. = FALSE
      )
    }
    transform_matrix <- dicom_iop_to_bonej_transform(dicom_iop)
    parsed_iop <- attr(transform_matrix, "dicom_iop")
  } else if (identical(transform_name, "flip_xy")) {
    transform_matrix <- diag(c(-1, -1, 1))
    parsed_iop <- NULL
  } else if (identical(transform_name, "none")) {
    transform_matrix <- diag(c(1, 1, 1))
    parsed_iop <- NULL
  } else if (identical(transform_name, "manual")) {
    if (is.null(bonej_transform_matrix)) {
      stop("`bonej_transform_matrix` is required when `bonej_coord_transform = 'manual'`.", call. = FALSE)
    }
    transform_matrix <- as.matrix(bonej_transform_matrix)
    if (!identical(dim(transform_matrix), c(3L, 3L)) || any(!is.finite(transform_matrix))) {
      stop("`bonej_transform_matrix` must be a finite 3 x 3 numeric matrix.", call. = FALSE)
    }
    parsed_iop <- NULL
  } else {
    stop(
      '`bonej_coord_transform` must be one of "dicom_iop", "flip_xy", "none", or "manual".',
      call. = FALSE
    )
  }

  rownames(transform_matrix) <- c("x", "y", "z")
  colnames(transform_matrix) <- c("stack_x", "stack_y", "stack_z")

  list(
    name = transform_name,
    matrix = transform_matrix,
    dicom_iop = parsed_iop
  )
}
