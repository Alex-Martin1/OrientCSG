# Internal camera geometry helper -------------------------------------------
#
# Construct an orthonormal, right-handed camera basis from a viewing direction
# and a preferred horizontal direction. `Z_axis` is interpreted as the viewing
# direction, whereas `X_axis` defines the intended screen-horizontal direction
# after projection onto the image plane. When available, `Y_preferred` is used to
# resolve the remaining sign ambiguity so that the image appears in the desired
# anatomical orientation.
make_camera_basis <- function(Z_axis, X_axis, Y_preferred = NULL) {
  Zcam <- nrm(Z_axis)
  Xcam <- X_axis - dot3(X_axis, Zcam) * Zcam

  if (sqrt(sum(Xcam^2)) < 1e-12) {
    if (is.null(Y_preferred)) {
      stop(
        "`X_axis` is collinear with `Z_axis`, and `Y_preferred` was not provided.",
        call. = FALSE
      )
    }
    Xcam <- cross3(Y_preferred, Zcam)
  }

  Xcam <- nrm(Xcam)
  Ycam <- nrm(cross3(Zcam, Xcam))
  Xcam <- nrm(cross3(Ycam, Zcam))

  if (!is.null(Y_preferred)) {
    Yp <- Y_preferred - dot3(Y_preferred, Zcam) * Zcam
    if (sqrt(sum(Yp^2)) > 1e-12 && dot3(Ycam, nrm(Yp)) < 0) {
      Xcam <- -Xcam
      Ycam <- -Ycam
    }
  }

  if (dot3(cross3(Xcam, Ycam), Zcam) < 0.999999) {
    stop(
      "The camera basis is not orthonormal and right-handed as expected.",
      call. = FALSE
    )
  }

  list(Xcam = Xcam, Ycam = Ycam, Zcam = Zcam)
}

# Internal camera geometry helper -------------------------------------------
#
# Convert a 3 x 3 rotation matrix into the axis-angle representation used by
# Avizo/Amira camera commands. The function includes explicit handling of the
# identity rotation and rotations close to 180 degrees, both of which are
# numerically sensitive special cases.
rotmat_to_axis_angle <- function(R) {
  if (!all(dim(R) == c(3, 3))) {
    stop("`R` must be a 3 x 3 matrix.", call. = FALSE)
  }

  tr <- R[1, 1] + R[2, 2] + R[3, 3]
  cos_theta <- (tr - 1) / 2
  cos_theta <- max(-1, min(1, cos_theta))
  theta <- acos(cos_theta)

  if (abs(theta) < 1e-12) {
    return(list(axis = c(0, 0, 1), angle = 0))
  }

  if (abs(pi - theta) < 1e-8) {
    axis <- c(
      sqrt(max(0, (R[1, 1] + 1) / 2)),
      sqrt(max(0, (R[2, 2] + 1) / 2)),
      sqrt(max(0, (R[3, 3] + 1) / 2))
    )
    if ((R[1, 2] + R[2, 1]) < 0) axis[2] <- -axis[2]
    if ((R[1, 3] + R[3, 1]) < 0) axis[3] <- -axis[3]
    if (sqrt(sum(axis^2)) < 1e-12) axis <- c(0, 0, 1)
    return(list(axis = nrm(axis), angle = theta))
  }

  s <- 2 * sin(theta)
  axis <- c(
    (R[3, 2] - R[2, 3]) / s,
    (R[1, 3] - R[3, 1]) / s,
    (R[2, 1] - R[1, 2]) / s
  )

  list(axis = nrm(axis), angle = theta)
}
