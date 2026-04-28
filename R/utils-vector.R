#' Normalize a numeric vector
#'
#' Low-level vector utility used internally by OrientCSG and exported for
#' convenience. `nrm()` returns the unit vector pointing in the same direction
#' as the input vector.
#'
#' @param v Numeric vector.
#'
#' @return A numeric vector with unit length.
#'
#' @examples
#' nrm(c(2, 0, 0))
#' nrm(c(1, 1, 0))
#'
#' @export
nrm <- function(v) {
  nv <- sqrt(sum(v * v))
  if (nv < 1e-15) stop("The vector has near-zero norm and cannot be normalized.", call. = FALSE)
  v / nv
}


#' Dot product for numeric vectors
#'
#' Low-level vector utility used internally by OrientCSG and exported for
#' convenience. `dot3()` computes the dot product between two numeric vectors.
#' In the orientation workflows, it is mainly used with 3D vectors.
#'
#' @param a,b Numeric vectors of the same length.
#'
#' @return A single numeric value giving the dot product of `a` and `b`.
#'
#' @examples
#' dot3(c(1, 2, 3), c(4, 5, 6))
#' dot3(c(1, 0, 0), c(0, 1, 0))
#'
#' @export
dot3 <- function(a, b) sum(a * b)


#' Cross product for 3D vectors
#'
#' Low-level 3D vector utility used internally by OrientCSG and exported for
#' convenience. `cross3()` computes the cross product between two 3D vectors.
#'
#' @param a,b Numeric vectors of length 3.
#'
#' @return A numeric vector of length 3 giving the cross product `a x b`.
#'
#' @examples
#' cross3(c(1, 0, 0), c(0, 1, 0))
#' cross3(c(0, 1, 0), c(1, 0, 0))
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
#' Low-level 3D utility used internally by OrientCSG and exported for
#' convenience. `dist3()` computes the Euclidean distance between two points in
#' three-dimensional space. The returned value is expressed in the same linear
#' unit as the input coordinates. For example, if the coordinates are given in
#' millimetres, the returned distance is also in millimetres.
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