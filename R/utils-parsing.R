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
    stop(sprintf("%s requiere %d números: %d landmarks x 3 coordenadas.", context, expected_n, n_landmarks), call. = FALSE)
  }
  if (length(nums) > expected_n) {
    warning(sprintf("Se han detectado %d números; se usarán solo los primeros %d.", length(nums), expected_n), call. = FALSE)
  }
  matrix(nums[seq_len(expected_n)], ncol = 3, byrow = TRUE)
}

parse_bonej_eigenvectors <- function(longitudinal_matrix_str) {
  nums <- extract_nums(longitudinal_matrix_str)
  if (length(nums) < 9) {
    stop("La matriz longitudinal debe contener al menos 9 números (3x3).", call. = FALSE)
  }
  matrix(nums[1:9], nrow = 3, byrow = TRUE)
}
