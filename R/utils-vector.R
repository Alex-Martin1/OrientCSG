#' Normalize a numeric vector
#'
#' Return the unit vector in the input direction.
#'
#' @param v Numeric vector.
#'
#' @return A numeric vector with unit length.
#'
#' @examples
#' nrm(c(2, 0, 0))
#'
#' @export
nrm <- function(v) {
  nv <- sqrt(sum(v * v))
  if (nv < 1e-15) stop("The vector has near-zero norm and cannot be normalized.", call. = FALSE)
  v / nv
}


#' Dot product for numeric vectors
#'
#' Compute the dot product of two numeric vectors.
#'
#' @param a,b Numeric vectors of the same length.
#'
#' @return A single numeric value giving the dot product of `a` and `b`.
#'
#' @examples
#' dot3(c(1, 2, 3), c(4, 5, 6))
#'
#' @export
dot3 <- function(a, b) sum(a * b)


#' Cross product for 3D vectors
#'
#' Compute the cross product of two 3D vectors.
#'
#' @param a,b Numeric vectors of length 3.
#'
#' @return A numeric vector of length 3 giving the cross product `a x b`.
#'
#' @examples
#' cross3(c(1, 0, 0), c(0, 1, 0))
#'
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
#' Compute Euclidean distance in the units of the input coordinates.
#'
#' @param point_a,point_b Numeric vectors of length 3.
#'
#' @return Euclidean distance between the two points.
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