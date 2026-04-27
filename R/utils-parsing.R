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
