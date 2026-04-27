reflect_point_across_plane <- function(P, A, B, C) {
  n <- cross3(B - A, C - A)
  if (sqrt(sum(n^2)) < 1e-12) {
    stop("The three points defining the reflection plane are collinear or nearly collinear.", call. = FALSE)
  }
  n <- nrm(n)
  P - 2 * dot3(P - A, n) * n
}

project_point_to_line <- function(P, A, B) {
  u <- B - A
  if (sqrt(sum(u^2)) < 1e-12) {
    stop("The projection line has near-zero length.", call. = FALSE)
  }
  u <- nrm(u)
  A + dot3(P - A, u) * u
}

project_vector_to_plane <- function(v, normal) {
  n <- nrm(normal)
  v - dot3(v, n) * n
}
