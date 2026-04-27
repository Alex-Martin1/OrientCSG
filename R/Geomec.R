#' Convert coordinate text to a landmark matrix
#'
#' This legacy helper parses a whitespace-separated coordinate string and
#' returns a matrix with three columns and one row per landmark.
#'
#' @param input_text Character string containing numeric coordinates.
#' @return A numeric matrix with columns x, y, and z; rows are labelled as LM1, LM2, etc.
#' @export
getmatrix <- function(input_text) {
  values <- strsplit(input_text, "\\s+")[[1]]
  values <- values[nzchar(values)]

  if (length(values) %% 3 != 0) {
    stop("The number of values must be a multiple of 3.", call. = FALSE)
  }

  values <- as.numeric(values)
  num_landmarks <- length(values) / 3
  result_matrix <- matrix(values, ncol = 3, byrow = TRUE)
  row.names(result_matrix) <- paste0("LM", seq_len(num_landmarks))
  result_matrix
}

#' Reflect LM1 across the plane defined by LM2, LM3, and LM4
#'
#' This legacy function computes LM1_Line as the mirror image of LM1 across
#' the plane defined by LM2, LM3, and LM4. For backward compatibility, it also
#' assigns `LM1_Line` and the augmented `result_matrix` to the global
#' environment.
#'
#' @param result_matrix Numeric landmark matrix. At least four landmarks are required.
#' @return Numeric vector with the three coordinates of LM1_Line.
#' @export
mirror_LM1 <- function(result_matrix) {
  if (nrow(result_matrix) < 4) {
    stop("At least four landmarks are required to compute the mirrored LM1.", call. = FALSE)
  }

  LM1 <- result_matrix[1, ]
  LM2 <- result_matrix[2, ]
  LM3 <- result_matrix[3, ]
  LM4 <- result_matrix[4, ]

  local_cross_product <- function(u, v) {
    c(
      u[2] * v[3] - u[3] * v[2],
      u[3] * v[1] - u[1] * v[3],
      u[1] * v[2] - u[2] * v[1]
    )
  }

  Vec_2_3 <- LM3 - LM2
  Vec_2_4 <- LM4 - LM2
  n <- local_cross_product(Vec_2_3, Vec_2_4)
  n_norm <- n / sqrt(sum(n^2))
  d <- sum((LM1 - LM2) * n_norm)
  LM1_Line <- LM1 - 2 * d * n_norm

  # Insert LM1_Line between LM4 and LM5.
  part1 <- result_matrix[1:4, , drop = FALSE]
  part2 <- result_matrix[5:nrow(result_matrix), , drop = FALSE]
  result_matrix <- rbind(part1, LM1_Line, part2)

  assign("LM1_Line", LM1_Line, envir = .GlobalEnv)
  assign("result_matrix", result_matrix, envir = .GlobalEnv)
  LM1_Line
}

#' Compute mandibular orientation vectors for the legacy protocol
#'
#' This legacy function computes LM0, the mandibular cross-section vectors
#' and the vector perpendicular to the alveolar reference plane. For backward
#' compatibility, selected outputs are assigned to the global environment.
#'
#' @param result_matrix Numeric landmark matrix including LM1_Line as row 5.
#' @return Matrix containing landmarks, computed vectors, and derived points.
#' @export
orthoplanes <- function(result_matrix) {
  LM1 <- result_matrix[1, ]
  LM2 <- result_matrix[2, ]
  LM3 <- result_matrix[3, ]
  LM4 <- result_matrix[4, ]
  LM1_Line <- result_matrix[5, ]
  CS1B <- result_matrix[6, ]
  LM6 <- result_matrix[7, ]
  CS2B <- result_matrix[8, ]
  LM8 <- result_matrix[9, ]
  LM9 <- result_matrix[10, ]
  LM10 <- result_matrix[11, ]
  LM11 <- result_matrix[12, ]

  Vec_1_1Line <- LM1_Line - LM1
  projection <- (sum((LM2 - LM1) * Vec_1_1Line) / sum(Vec_1_1Line^2)) * Vec_1_1Line
  LM0 <- LM1 + projection
  Vec_0_2 <- LM2 - LM0

  Vec_CS1 <- LM6 - CS1B
  Vec_CS2 <- LM8 - CS2B

  # Compute Vec_Penp, the unit vector perpendicular to the ARP.
  v1 <- LM2 - LM1
  v2 <- LM1_Line - LM1

  Vec_Penp_raw <- c(
    v1[2] * v2[3] - v1[3] * v2[2],
    v1[3] * v2[1] - v1[1] * v2[3],
    v1[1] * v2[2] - v1[2] * v2[1]
  )

  Vec_Penp <- Vec_Penp_raw / sqrt(sum(Vec_Penp_raw^2))

  assign("Vec_0_2", Vec_0_2, envir = .GlobalEnv)
  assign("Vec_CS1", Vec_CS1, envir = .GlobalEnv)
  assign("Vec_CS2", Vec_CS2, envir = .GlobalEnv)
  assign("Vec_Penp", Vec_Penp, envir = .GlobalEnv)

  result_matrix <- rbind(
    LM1, LM2, LM3, LM4, LM1_Line, CS1B, Vec_CS1, CS2B, Vec_CS2,
    Vec_Penp, Vec_0_2, Vec_1_1Line, LM0, LM6, LM8, LM9, LM10, LM11
  )
  assign("result_matrix", result_matrix, envir = .GlobalEnv)
  result_matrix
}

#' Compute mandibular linear measurements
#'
#' Computes the linear measurements used in the legacy mandibular
#' standardization protocol.
#'
#' @param result_matrix Numeric matrix returned by `orthoplanes()`.
#' @return Data frame containing mandibular linear measurements in millimetres.
#' @export
mandibular_metrics <- function(result_matrix) {
  coords <- result_matrix
  LM1 <- coords[1, ]
  LM2 <- coords[2, ]
  LM3 <- coords[3, ]          # Gnathion.
  LM4 <- coords[4, ]
  LM1_Line <- coords[5, ]
  LM9 <- coords[16, ]         # Bigonial angle.
  LM10 <- coords[17, ]        # C-P3.
  LM11 <- coords[18, ]        # Condyle midpoint.
  LM0 <- coords[13, ]

  euclidean_distance <- function(p1, p2) {
    sqrt(sum((p1 - p2)^2))
  }

  local_cross_product <- function(u, v) {
    c(
      u[2] * v[3] - u[3] * v[2],
      u[3] * v[1] - u[1] * v[3],
      u[1] * v[2] - u[2] * v[1]
    )
  }

  dot_product <- function(u, v) {
    sum(u * v)
  }

  normalize_vector <- function(v) {
    v / sqrt(sum(v^2))
  }

  mandibular_lateral_length <- euclidean_distance(LM10, LM11)
  corpus_length <- euclidean_distance(LM9, LM3)
  estimated_dental_arch_breadth <- euclidean_distance(LM1, LM1_Line)
  estimated_dental_arch_length <- euclidean_distance(LM2, LM0)

  # Estimate bigonial breadth by reflecting LM9 across plane P.
  B <- LM2
  C <- LM3
  D <- LM4
  BC <- C - B
  BD <- D - B
  n <- local_cross_product(BC, BD)
  n_unit <- normalize_vector(n)
  d <- dot_product((LM9 - B), n_unit)
  LM9_prime <- LM9 - 2 * d * n_unit
  estimated_bigonial_breadth <- euclidean_distance(LM9, LM9_prime)

  result <- data.frame(
    `Mandibular lateral length` = mandibular_lateral_length,
    `Corpus length` = corpus_length,
    `Estimated dental arch breadth` = estimated_dental_arch_breadth,
    `Estimated dental arch length` = estimated_dental_arch_length,
    `Estimated bigonial breadth` = estimated_bigonial_breadth
  )

  assign("mandibular_metrics", result, envir = .GlobalEnv)
  result
}

#' Compute legacy point and vector matrix
#'
#' Computes LM0, section vectors, and the ARP-perpendicular vector following
#' the earlier Godinho et al. (2020)-oriented workflow. For backward
#' compatibility, selected outputs are assigned to the global environment.
#'
#' @param result_matrix Numeric landmark matrix including LM1_Line as row 5.
#' @return Matrix containing landmarks, computed vectors, and derived points.
#' @export
pMV <- function(result_matrix) {
  LM1 <- result_matrix[1, ]
  LM2 <- result_matrix[2, ]
  LM3 <- result_matrix[3, ]
  LM4 <- result_matrix[4, ]
  LM1_Line <- result_matrix[5, ]
  CS1B <- result_matrix[6, ]
  LM6 <- result_matrix[7, ]
  CS2B <- result_matrix[8, ]
  LM8 <- result_matrix[9, ]

  Vec_1_1Line <- LM1_Line - LM1
  projection <- (sum((LM2 - LM1) * Vec_1_1Line) / sum(Vec_1_1Line^2)) * Vec_1_1Line
  LM0 <- LM1 + projection
  Vec_0_2 <- LM2 - LM0

  Vec_CS1 <- LM6 - CS1B
  Vec_CS2 <- LM8 - CS2B

  # Compute Vec_Penp, the unit vector perpendicular to the ARP.
  v1 <- LM2 - LM1
  v2 <- LM1_Line - LM1

  Vec_Penp_raw <- c(
    v1[2] * v2[3] - v1[3] * v2[2],
    v1[3] * v2[1] - v1[1] * v2[3],
    v1[1] * v2[2] - v1[2] * v2[1]
  )

  Vec_Penp <- Vec_Penp_raw / sqrt(sum(Vec_Penp_raw^2))

  assign("Vec_0_2", Vec_0_2, envir = .GlobalEnv)
  assign("Vec_CS1", Vec_CS1, envir = .GlobalEnv)
  assign("Vec_CS2", Vec_CS2, envir = .GlobalEnv)
  assign("Vec_Penp", Vec_Penp, envir = .GlobalEnv)

  result_matrix <- rbind(
    LM1, LM2, LM3, LM4, LM1_Line, CS1B, Vec_CS1, CS2B, Vec_CS2,
    Vec_Penp, Vec_0_2, Vec_1_1Line, LM0, LM6, LM8
  )
  assign("result_matrix", result_matrix, envir = .GlobalEnv)
  result_matrix
}

#' Compute legacy mandibular distances
#'
#' Computes mandibular distances used in the earlier Godinho et al.
#' (2020)-oriented workflow. The function preserves the historical side
#' effects by assigning `coords_11_prime`, `dist_11_11prime`, and
#' `distances_matrix` to the global environment.
#'
#' @param input_text Character string containing landmark coordinates.
#' @return Matrix containing measurement names and values.
#' @export
distR <- function(input_text) {
  vals <- as.numeric(strsplit(input_text, "\\s+")[[1]])
  vals <- vals[!is.na(vals)]
  if (length(vals) %% 3 != 0) {
    stop("The number of values must be a multiple of 3.", call. = FALSE)
  }
  coords <- matrix(vals, ncol = 3, byrow = TRUE)

  # Reflect landmark 11 across the plane defined by landmarks 23, 24, and 25.
  P11 <- coords[11, ]
  B <- coords[23, ]
  C <- coords[24, ]
  D <- coords[25, ] # Infradentale projection.

  local_cross_product <- function(u, v) {
    c(
      u[2] * v[3] - u[3] * v[2],
      u[3] * v[1] - u[1] * v[3],
      u[1] * v[2] - u[2] * v[1]
    )
  }

  BC <- C - B
  BD <- D - B
  n <- local_cross_product(BC, BD)
  n_unit <- n / sqrt(sum(n^2))
  d <- sum((P11 - B) * n_unit)

  P11_proj <- P11 - 2 * d * n_unit
  dist_11_11prime <- sqrt(sum((P11 - P11_proj)^2))

  assign("coords_11_prime", P11_proj, envir = .GlobalEnv)
  assign("dist_11_11prime", dist_11_11prime, envir = .GlobalEnv)

  dist_names <- c(
    "Mandibular lateral length",
    "Corpus length",
    "Dental arcade breadth",
    "Mandibular superior length",
    "Estimated bigonial breadth"
  )
  dist_vals <- c(
    sqrt(sum((coords[6, ] - coords[13, ])^2)),
    sqrt(sum((coords[1, ] - coords[11, ])^2)),
    sqrt(sum((coords[22, ] - coords[26, ])^2)),
    sqrt(sum((coords[23, ] - coords[27, ])^2)),
    dist_11_11prime
  )

  result <- matrix(
    c(dist_names, as.character(dist_vals)),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("Measurements", "Values"), NULL)
  )

  assign("distances_matrix", result, envir = .GlobalEnv)
  result
}

#' Calculate the distance between two 3D points
#'
#' @param point_A Numeric vector with three coordinates.
#' @param point_B Numeric vector with three coordinates.
#' @return Euclidean distance between the two points. The value is also printed.
#' @export
PointDistancemm <- function(point_A, point_B) {
  distance <- sqrt(sum((point_A - point_B)^2))
  cat("The distance between point A and point B is:", distance, "\n")
  invisible(distance)
}

#' Convert coordinate text to a numeric vector
#'
#' @param input_text Character string containing numeric values.
#' @return Numeric vector.
#' @export
text2vector <- function(input_text) {
  lines <- strsplit(input_text, "\n", fixed = TRUE)[[1]]
  values <- numeric(0)

  for (line in lines) {
    coords <- strsplit(line, " ")[[1]]
    coords <- as.numeric(trimws(coords))
    values <- c(values, coords)
  }

  values
}

#' Compute the cross product between two vectors
#'
#' @param u_vector Numeric vector of length 3.
#' @param v_vector Numeric vector of length 3.
#' @return Numeric vector of length 3 with the cross product.
#' @export
cross_product <- function(u_vector, v_vector) {
  c(
    u_vector[2] * v_vector[3] - u_vector[3] * v_vector[2],
    u_vector[3] * v_vector[1] - u_vector[1] * v_vector[3],
    u_vector[1] * v_vector[2] - u_vector[2] * v_vector[1]
  )
}
