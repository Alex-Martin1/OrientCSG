# Internal mesh inertia helper -----------------------------------------------
#
# Compute mass properties of a closed triangular surface mesh by decomposing the
# enclosed solid into signed tetrahedra with the origin. The eigenvector
# associated with the smallest rotational inertia is used as the longitudinal
# axis for elongated bones.
compute_mesh_inertia_axes <- function(mesh_file,
                                      clean = FALSE,
                                      stabilize_first_axis_negative_z = TRUE) {
  if (!requireNamespace("Rvcg", quietly = TRUE)) {
    stop(
      "Package 'Rvcg' is required for `SOLID = TRUE`. Install it with install.packages('Rvcg').",
      call. = FALSE
    )
  }

  if (missing(mesh_file) || length(mesh_file) != 1L || is.na(mesh_file) || !nzchar(mesh_file)) {
    stop("`mesh_file` must be a path to a closed surface mesh.", call. = FALSE)
  }

  if (!file.exists(mesh_file)) {
    stop("Mesh file does not exist: ", mesh_file, call. = FALSE)
  }

  mesh <- Rvcg::vcgImport(mesh_file, clean = clean, readcolor = FALSE, silent = TRUE)

  if (is.null(mesh$vb) || is.null(mesh$it)) {
    stop("The imported mesh does not contain vertices and triangular faces.", call. = FALSE)
  }

  vertices <- t(mesh$vb[1:3, , drop = FALSE])
  faces <- t(mesh$it[1:3, , drop = FALSE])

  mass <- 0
  first_moment <- c(0, 0, 0)
  second_moment <- matrix(0, nrow = 3, ncol = 3)

  for (i in seq_len(nrow(faces))) {
    a <- vertices[faces[i, 1], ]
    b <- vertices[faces[i, 2], ]
    c <- vertices[faces[i, 3], ]

    V <- dot3(a, cross3(b, c)) / 6
    S <- a + b + c

    first_moment <- first_moment + V * S / 4

    Q <- outer(a, a) +
      outer(b, b) +
      outer(c, c) +
      outer(S, S)

    second_moment <- second_moment + V * Q / 20
    mass <- mass + V
  }

  if (abs(mass) < .Machine$double.eps) {
    stop(
      "Computed mesh volume is near zero. The mesh may be open, badly oriented, or not closed.",
      call. = FALSE
    )
  }

  if (mass < 0) {
    mass <- -mass
    first_moment <- -first_moment
    second_moment <- -second_moment
  }

  centroid <- first_moment / mass
  central_second <- second_moment - mass * outer(centroid, centroid)
  inertia_tensor <- sum(diag(central_second)) * diag(3) - central_second

  eig <- eigen(inertia_tensor, symmetric = TRUE)
  ord <- order(eig$values)

  eigenvalues <- eig$values[ord]
  eigenvectors <- eig$vectors[, ord, drop = FALSE]

  if (isTRUE(stabilize_first_axis_negative_z) && eigenvectors[3, 1] > 0) {
    eigenvectors[, 1] <- -eigenvectors[, 1]
  }

  if (det(eigenvectors) < 0) {
    eigenvectors[, 3] <- -eigenvectors[, 3]
  }

  colnames(eigenvectors) <- c("axis_min_inertia", "axis_mid_inertia", "axis_max_inertia")
  rownames(eigenvectors) <- c("x", "y", "z")

  list(
    volume = mass,
    centroid = centroid,
    inertia_tensor = inertia_tensor,
    eigenvalues = eigenvalues,
    eigenvectors = eigenvectors
  )
}
