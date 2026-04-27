# Internal plane geometry helper --------------------------------------------
#
# Reflect point P across the plane defined by three points A, B, and C. In the
# mandibular workflow this operation is used to estimate contralateral reference
# points, such as LM1_Line and LM9_Line, from a preserved hemimandible. The
# function assumes that A, B, and C define a stable plane; nearly collinear points
# are rejected because they do not define a reliable reflection plane.
reflect_point_across_plane <- function(P, A, B, C) {
  n <- cross3(B - A, C - A)

  if (sqrt(sum(n^2)) < 1e-12) {
    stop(
      "The three points defining the reflection plane are collinear or nearly collinear.",
      call. = FALSE
    )
  }

  n <- nrm(n)
  P - 2 * dot3(P - A, n) * n
}

# Internal plane geometry helper --------------------------------------------
#
# Project point P orthogonally onto the line defined by points A and B. In the
# mandibular protocol, this is used to compute LM0 as the projection of LM2 onto
# the LM1--LM1_Line direction. The projected point provides a stable reference
# for defining the anterior direction of the alveolar reference plane.
project_point_to_line <- function(P, A, B) {
  u <- B - A

  if (sqrt(sum(u^2)) < 1e-12) {
    stop("The projection line has near-zero length.", call. = FALSE)
  }

  u <- nrm(u)
  A + dot3(P - A, u) * u
}

# Internal plane geometry helper --------------------------------------------
#
# Remove from vector v the component that is parallel to a plane normal. The
# result is the projection of v onto the plane perpendicular to `normal`. This
# operation is used to force anatomical axes to lie within the intended section
# plane when a raw landmark-defined direction is not exactly orthogonal to the
# main reference axis.
project_vector_to_plane <- function(v, normal) {
  n <- nrm(normal)
  v - dot3(v, n) * n
}
