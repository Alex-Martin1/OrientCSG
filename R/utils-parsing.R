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
# and IDs are mixed with coordinate values.
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
# Resolve the preferred coordinate-system argument and its legacy alias.
resolve_lm_coord_system <- function(lm_coord_system = "LPS",
                                    landmark_coordinate_system = NULL,
                                    lm_coord_system_missing = FALSE) {
  if (!is.null(landmark_coordinate_system)) {
    old_value <- toupper(trimws(landmark_coordinate_system))
    new_value <- toupper(trimws(lm_coord_system))

    if (!lm_coord_system_missing && !identical(new_value, old_value)) {
      stop(
        "`lm_coord_system` and `landmark_coordinate_system` were both supplied but differ.",
        call. = FALSE
      )
    }

    lm_coord_system <- landmark_coordinate_system
  }

  lm_coord_system <- toupper(trimws(lm_coord_system))
  if (length(lm_coord_system) != 1L || is.na(lm_coord_system) || !lm_coord_system %in% c("LPS", "RAS")) {
    stop('`lm_coord_system` must be "LPS" or "RAS".', call. = FALSE)
  }

  lm_coord_system
}

# Internal argument helper ---------------------------------------------------
#
# Resolve the preferred landmark text argument and its legacy Slicer alias.
resolve_landmarks_str <- function(landmarks_str = NULL, slicer_landmarks_str = NULL) {
  if (!is.null(landmarks_str) && !is.null(slicer_landmarks_str)) {
    if (!identical(trimws(landmarks_str), trimws(slicer_landmarks_str))) {
      stop(
        "Both `landmarks_str` and `slicer_landmarks_str` were supplied, but they differ.",
        call. = FALSE
      )
    }
  }

  if (is.null(landmarks_str)) {
    landmarks_str <- slicer_landmarks_str
  }

  if (is.null(landmarks_str) || length(landmarks_str) != 1L || !nzchar(trimws(landmarks_str))) {
    stop("`landmarks_str` is required.", call. = FALSE)
  }

  landmarks_str
}

# Internal coordinate helper -------------------------------------------------
#
# Convert input landmark coordinates to the package's internal LPS/external
# convention. The textual input format is deliberately handled elsewhere: this
# function only interprets the spatial coordinate system declared by the user.
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

# Internal parsing helper ----------------------------------------------------
#
# Backward-compatible wrapper for older internal code paths. This function no
# longer owns coordinate-system semantics; it delegates textual parsing to the
# flexible parser and then applies the explicitly declared coordinate system.
parse_slicer_landmarks <- function(slicer_landmarks_str,
                                   mode = "TIBIA",
                                   coordinate_system = "LPS") {
  mode <- toupper(trimws(mode))

  n_landmarks <- switch(
    mode,
    TIBIA = 3L,
    HUMERUS = 4L,
    stop('Slicer landmark parsing is implemented only for `mode = "TIBIA"` and `mode = "HUMERUS"`.', call. = FALSE)
  )

  mat <- parse_landmarks(slicer_landmarks_str, n_landmarks = n_landmarks, context = mode)
  normalize_lm_coordinates(mat, lm_coord_system = coordinate_system, arg = "landmark_coordinate_system")
}
