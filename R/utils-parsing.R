#' Extract numbers from a free-form coordinate string
#'
#' @param txt Character string containing numeric values.
#' @return Numeric vector.
#' @export
extract_nums <- function(txt) {
  clean <- gsub("[|,;]", " ", txt)
  scan(text = clean, quiet = TRUE)
}

parse_landmarks <- function(landmarks_str, n_landmarks, context = "landmarks") {
  nums <- extract_nums(landmarks_str)
  expected_n <- n_landmarks * 3
  if (length(nums) < expected_n) {
    stop(sprintf("%s requires %d numeric values: %d landmarks x 3 coordinates.", context, expected_n, n_landmarks), call. = FALSE)
  }
  if (length(nums) > expected_n) {
    warning(sprintf("%d numeric values were detected; only the first %d will be used.", length(nums), expected_n), call. = FALSE)
  }
  matrix(nums[seq_len(expected_n)], ncol = 3, byrow = TRUE)
}

parse_bonej_eigenvectors <- function(longitudinal_matrix_str) {
  nums <- extract_nums(longitudinal_matrix_str)
  if (length(nums) < 9) {
    stop("The longitudinal matrix must contain at least 9 numeric values (3 x 3).", call. = FALSE)
  }
  matrix(nums[1:9], nrow = 3, byrow = TRUE)
}
