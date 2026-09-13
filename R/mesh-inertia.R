# Internal mesh inertia helper -----------------------------------------------
#
# Compute mass properties of a closed triangular surface mesh by decomposing the
# enclosed solid into signed tetrahedra with the origin. The eigenvector
# associated with the smallest rotational inertia is used as the longitudinal
# axis for elongated bones.
compute_mesh_inertia_axes <- function(mesh_file,
                                      clean = FALSE,
                                      stabilize_first_axis_negative_z = TRUE,
                                      chunk_size = 250000L) {
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

  chunk_size <- as.integer(chunk_size)
  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }

  # Vertex normals are not used for the solid mass-property calculation, so
  # avoid the potentially expensive normal recomputation during import.
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

  # The original implementation accumulated the same signed-tetrahedron
  # expressions one face at a time in R. Process faces in vectorized chunks
  # instead. This preserves the same solid integral while avoiding millions of
  # interpreted R-loop iterations and large full-mesh transpose copies.
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

    # Sum V * [aa' + bb' + cc' + SS'] / 20 without constructing
    # one 3 x 3 matrix per triangle. crossprod() performs the heavy work in
    # compiled code.
    second_moment <- second_moment + (
      crossprod(a, a * V) +
        crossprod(b, b * V) +
        crossprod(c, c * V) +
        crossprod(S, S * V)
    ) / 20
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
