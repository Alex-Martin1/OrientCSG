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
# Convert a landmark coordinate string into an n x 3 numeric matrix. The number
# of expected landmarks is set by the calling workflow, because mandibular and
# long-bone protocols use different landmark sets.
parse_landmarks <- function(landmarks_str, n_landmarks, context = "landmarks") {
  nums <- extract_nums(landmarks_str)
  expected_n <- n_landmarks * 3

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

  matrix(nums[seq_len(expected_n)], ncol = 3, byrow = TRUE)
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
# Extract numeric tokens from text that may also contain labels such as F-1.
# This is used for 3D Slicer Markups tables copied as plain text.
extract_numeric_tokens <- function(txt) {
  pattern <- "[-+]?(?:\\d*\\.\\d+|\\d+\\.?\\d*)(?:[eE][-+]?\\d+)?"
  matches <- gregexpr(pattern, txt, perl = TRUE)
  out <- regmatches(txt, matches)[[1]]
  as.numeric(out)
}

# Internal coordinate helper -------------------------------------------------
#
# Convert between RAS and LPS/external conventions by inverting X and Y. The
# operation is its own inverse, so the same helper is used in both directions.
flip_xy <- function(x) {
  x <- as.numeric(x)
  c(-x[1], -x[2], x[3])
}

# Internal parsing helper ----------------------------------------------------
#
# Read tibial landmarks copied from a 3D Slicer Markups table. Recognition is
# positional, matching the protocol rather than relying on labels in the table.
# The fixed Slicer row order is:
#   row 1 = Plateau2
#   row 2 = Plateau1
#   row 3 = TibioTalar
# The returned matrix is reordered internally as Plateau1, Plateau2, TibioTalar.
parse_slicer_landmarks <- function(slicer_landmarks_str,
                                   coordinate_system = "LPS") {
  lines <- unlist(strsplit(slicer_landmarks_str, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]

  landmark_order <- c("Plateau2", "Plateau1", "TibioTalar")
  coord_cols <- c(2, 3, 4)

  if (length(lines) < length(landmark_order)) {
    stop("Fewer Slicer landmark rows than expected for tibia.", call. = FALSE)
  }

  mat <- matrix(NA_real_, nrow = length(landmark_order), ncol = 3)
  colnames(mat) <- c("x", "y", "z")
  rownames(mat) <- landmark_order

  for (i in seq_along(landmark_order)) {
    vals <- extract_numeric_tokens(lines[i])

    if (length(vals) < max(coord_cols)) {
      stop(
        sprintf("Slicer landmark row %d does not contain enough numeric columns.", i),
        call. = FALSE
      )
    }

    mat[i, ] <- vals[coord_cols]
  }

  coordinate_system <- toupper(trimws(coordinate_system))
  if (coordinate_system == "RAS") {
    mat <- t(apply(mat, 1, flip_xy))
    colnames(mat) <- c("x", "y", "z")
    rownames(mat) <- landmark_order
  } else if (coordinate_system != "LPS") {
    stop('`landmark_coordinate_system` must be "LPS" or "RAS".', call. = FALSE)
  }

  mat[c("Plateau1", "Plateau2", "TibioTalar"), , drop = FALSE]
}
