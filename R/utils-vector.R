#' Normalize a numeric vector
#'
#' @param v Numeric vector.
#' @return Unit vector.
#' @export
nrm <- function(v) {
  nv <- sqrt(sum(v * v))
  if (nv < 1e-15) stop("The vector has near-zero norm and cannot be normalized.", call. = FALSE)
  v / nv
}

#' Dot product for 3D vectors
#'
#' @param a,b Numeric vectors.
#' @return Numeric scalar.
#' @export
dot3 <- function(a, b) sum(a * b)

#' Cross product for 3D vectors
#'
#' @param a,b Numeric vectors of length 3.
#' @return Numeric vector of length 3.
#' @export
cross3 <- function(a, b) {
  c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  )
}

#' Compute the Euclidean distance between two 3D points
#'
#' `dist3()` computes the Euclidean distance between two points in
#' three-dimensional space. The returned value is expressed in the same
#' linear unit as the input coordinates. For example, if the coordinates
#' are given in millimetres, the returned distance is also in millimetres.
#'
#' @param point_a Numeric vector of length 3 with the coordinates of the first point.
#' @param point_b Numeric vector of length 3 with the coordinates of the second point.
#'
#' @return A single numeric value giving the Euclidean distance between `point_a` and `point_b`.
#'
#' @examples
#' dist3(c(1, 2, 3), c(5, 5, 3))
#'
#' @export
dist3 <- function(point_a, point_b) {
  point_a <- as.numeric(point_a)
  point_b <- as.numeric(point_b)
  
  if (length(point_a) != 3L || length(point_b) != 3L) {
    stop("`point_a` and `point_b` must both be numeric vectors of length 3.", call. = FALSE)
  }
  
  sqrt(sum((point_b - point_a)^2))
}
