# Signed-tetrahedron mass properties; the minimum-inertia eigenvector is the
# longitudinal axis. Large/asymmetric mesh gaps can bias this estimate.
compute_mesh_inertia_axes <- function(mesh_file,
                                      clean = FALSE,
                                      stabilize_first_axis_negative_z = TRUE,
                                      chunk_size = 250000L) {
  if (!requireNamespace("Rvcg", quietly = TRUE)) {
    stop(
      "Package 'Rvcg' is required for `INPUT = \"MESH\"`. Install it with install.packages('Rvcg').",
      call. = FALSE
    )
  }

  if (missing(mesh_file) || length(mesh_file) != 1L || is.na(mesh_file) || !nzchar(mesh_file)) {
    stop("`mesh_file` must be a path to a triangular surface mesh.", call. = FALSE)
  }

  if (!file.exists(mesh_file)) {
    stop("Mesh file does not exist: ", mesh_file, call. = FALSE)
  }

  chunk_size <- as.integer(chunk_size)
  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }

  # Normals are unnecessary for mass properties.
  mesh <- Rvcg::vcgImport(
    mesh_file,
    updateNormals = FALSE,
    clean = clean,
    readcolor = FALSE,
    silent = TRUE
  )

  if (is.null(mesh$vb) || is.null(mesh$it)) {
    stop("The imported mesh does not contain vertices and triangular faces.", call. = FALSE)
  }

  n_faces <- ncol(mesh$it)
  if (is.null(n_faces) || n_faces < 1L) {
    stop("The imported mesh does not contain triangular faces.", call. = FALSE)
  }

  mass <- 0
  first_moment <- c(0, 0, 0)
  second_moment <- matrix(0, nrow = 3, ncol = 3)

  # Vectorize over face chunks to limit memory use.
  starts <- seq.int(1L, n_faces, by = chunk_size)

  for (start in starts) {
    end <- min(start + chunk_size - 1L, n_faces)
    idx <- start:end

    f1 <- mesh$it[1L, idx]
    f2 <- mesh$it[2L, idx]
    f3 <- mesh$it[3L, idx]

    a <- t(mesh$vb[1:3, f1, drop = FALSE])
    b <- t(mesh$vb[1:3, f2, drop = FALSE])
    c <- t(mesh$vb[1:3, f3, drop = FALSE])

    # Row-wise dot(a, cross(b, c)) / 6.
    volume6 <-
      a[, 1L] * (b[, 2L] * c[, 3L] - b[, 3L] * c[, 2L]) +
      a[, 2L] * (b[, 3L] * c[, 1L] - b[, 1L] * c[, 3L]) +
      a[, 3L] * (b[, 1L] * c[, 2L] - b[, 2L] * c[, 1L])

    V <- volume6 / 6
    S <- a + b + c

    mass <- mass + sum(V)
    first_moment <- first_moment + colSums(S * V) / 4

    # Second moments: sum V * [aa' + bb' + cc' + SS'] / 20.
    second_moment <- second_moment + (
      crossprod(a, a * V) +
        crossprod(b, b * V) +
        crossprod(c, c * V) +
        crossprod(S, S * V)
    ) / 20
  }

  if (abs(mass) < .Machine$double.eps) {
    stop(
      "Computed mesh volume is near zero. The mesh may be severely incomplete, degenerate, badly oriented, or otherwise unsuitable for volumetric inertia estimation.",
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
