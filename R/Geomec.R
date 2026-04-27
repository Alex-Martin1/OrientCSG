#Transform text input into a matrix with all the points labeled
getmatrix <- function(input_text) {
  values <- strsplit(input_text, "\\s+")[[1]]
  if (length(values) %% 3 != 0) {
    stop("The number of values must be a multiple of 3.")
  }
  values <- as.numeric(values)
  num_landmarks <- length(values) / 3
  result_matrix <- matrix(values, ncol = 3, byrow = TRUE)
  row.names(result_matrix) <- paste0("LM", seq_len(num_landmarks))
  return(result_matrix)
}



#Mirror Landmark 1 function
mirror_LM1 <- function(result_matrix) {
  if (nrow(result_matrix) < 4) {
    stop("At least 4 landmarks are required to compute the mirrored LM1.")
  }
  
  LM1 <- result_matrix[1, ]
  LM2 <- result_matrix[2, ]
  LM3 <- result_matrix[3, ]
  LM4 <- result_matrix[4, ]
  
  cross_product <- function(u, v) {
    c(u[2]*v[3] - u[3]*v[2],
      u[3]*v[1] - u[1]*v[3],
      u[1]*v[2] - u[2]*v[1])
  }
  
  Vec_2_3 <- LM3 - LM2
  Vec_2_4 <- LM4 - LM2
  n <- cross_product(Vec_2_3, Vec_2_4)
  n_norm <- n / sqrt(sum(n^2))
  d <- sum((LM1 - LM2) * n_norm)
  LM1_Line <- LM1 - 2 * d * n_norm
  
  # Inserta LM1_Line entre LM4 y LM5
  part1 <- result_matrix[1:4, , drop = FALSE]
  part2 <- result_matrix[5:nrow(result_matrix), , drop = FALSE]
  result_matrix <- rbind(part1, LM1_Line, part2)
  
  assign("LM1_Line", LM1_Line, envir = .GlobalEnv)
  assign("result_matrix", result_matrix, envir = .GlobalEnv)
  return(LM1_Line)
}



#Create point 0 and perpendicular vectors function
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

#Calculation of Vec_Penp  
v1 <- LM2 - LM1
v2 <- LM1_Line - LM1

Vec_Penp_raw <- c(
  v1[2]*v2[3] - v1[3]*v2[2],
  v1[3]*v2[1] - v1[1]*v2[3],
  v1[1]*v2[2] - v1[2]*v2[1]
)

Vec_Penp <- Vec_Penp_raw / sqrt(sum(Vec_Penp_raw^2))  # Normalized

#Results
assign("Vec_0_2", Vec_0_2, envir = .GlobalEnv)
assign("Vec_CS1", Vec_CS1, envir = .GlobalEnv)
assign("Vec_CS2", Vec_CS2, envir = .GlobalEnv)
assign("Vec_Penp", Vec_Penp, envir = .GlobalEnv)

result_matrix <- rbind(LM1, LM2, LM3, LM4, LM1_Line, CS1B, Vec_CS1, CS2B, Vec_CS2, Vec_Penp, Vec_0_2, Vec_1_1Line, LM0, LM6, LM8, LM9, LM10, LM11)
assign("result_matrix", result_matrix, envir = .GlobalEnv)
return(result_matrix)
}




#Calculate euclidean distance in mm between points - Standard protocol
mandibular_metrics <- function(result_matrix) {
  # Extraer coordenadas
  coords <- result_matrix
  LM1 <- coords[1, ]
  LM2 <- coords[2, ]
  LM3 <- coords[3, ]          # Gnathion
  LM4 <- coords[4, ]
  LM1_Line <- coords[5, ]
  LM9 <- coords[16, ]         # Bigonial angle
  LM10 <- coords[17, ]        # C-P3
  LM11 <- coords[18, ]        # Condyle midpoint
  LM0 <- coords[13, ]
  
  # Función auxiliar para calcular distancia euclidiana
  euclidean_distance <- function(p1, p2) {
    sqrt(sum((p1 - p2)^2))
  }
  
  # Producto vectorial
  cross_product <- function(u, v) {
    c(
      u[2]*v[3] - u[3]*v[2],
      u[3]*v[1] - u[1]*v[3],
      u[1]*v[2] - u[2]*v[1]
    )
  }
  
  # Producto escalar
  dot_product <- function(u, v) {
    sum(u * v)
  }
  
  # Normalización de vector
  normalize_vector <- function(v) {
    v / sqrt(sum(v^2))
  }
  
  # 1. Mandibular lateral length: LM10 - LM11
  mandibular_lateral_length <- euclidean_distance(LM10, LM11)
  
  # 2. Corpus length: LM9 - LM3
  corpus_length <- euclidean_distance(LM9, LM3)
  
  # 3. Estimated dental arch breadth: LM1 - LM1_Line
  estimated_dental_arch_breadth <- euclidean_distance(LM1, LM1_Line)
  
  # 4. Estimated dental arch length: LM2 - LM0
  estimated_dental_arch_length <- euclidean_distance(LM2, LM0)
  
  # 5. Estimated bigonial breadth (reflexión de LM9 respecto al plano P)
  B <- LM2
  C <- LM3
  D <- LM4
  BC <- C - B
  BD <- D - B
  n <- cross_product(BC, BD)
  n_unit <- normalize_vector(n)
  d <- dot_product((LM9 - B), n_unit)
  LM9_prime <- LM9 - 2 * d * n_unit
  estimated_bigonial_breadth <- euclidean_distance(LM9, LM9_prime)
  
  # Construcción de la tabla de salida
  result <- data.frame(
    `Mandibular lateral length`       = mandibular_lateral_length,
    `Corpus length`                   = corpus_length,
    `Estimated dental arch breadth`  = estimated_dental_arch_breadth,
    `Estimated dental arch length`   = estimated_dental_arch_length,
    `Estimated bigonial breadth`     = estimated_bigonial_breadth
  )
  
  assign("mandibular_metrics", result, envir = .GlobalEnv)
  
  return(result)
}



#ALEX - Create point M function - Godinho et al., 2020 protocol
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
  
  #Calculation of Vec_Penp  
  v1 <- LM2 - LM1
  v2 <- LM1_Line - LM1
  
  Vec_Penp_raw <- c(
    v1[2]*v2[3] - v1[3]*v2[2],
    v1[3]*v2[1] - v1[1]*v2[3],
    v1[1]*v2[2] - v1[2]*v2[1]
  )
  
  Vec_Penp <- Vec_Penp_raw / sqrt(sum(Vec_Penp_raw^2))  # Normalized
  
  #Results
  assign("Vec_0_2", Vec_0_2, envir = .GlobalEnv)
  assign("Vec_CS1", Vec_CS1, envir = .GlobalEnv)
  assign("Vec_CS2", Vec_CS2, envir = .GlobalEnv)
  assign("Vec_Penp", Vec_Penp, envir = .GlobalEnv)
  
  result_matrix <- rbind(LM1, LM2, LM3, LM4, LM1_Line, CS1B, Vec_CS1, CS2B, Vec_CS2, Vec_Penp, Vec_0_2, Vec_1_1Line, LM0, LM6, LM8)
  assign("result_matrix", result_matrix, envir = .GlobalEnv)
  return(result_matrix)
}

#ALEX - Calculate euclidean distance in mm between points function - Godinho et al., 2020 protocol
distR <- function(input_text) {
  # 1) Parseo de las coordenadas
  vals <- as.numeric(strsplit(input_text, "\\s+")[[1]])
  if (length(vals) %% 3 != 0)
    stop("The number of values must be a multiple of 3")
  coords <- matrix(vals, ncol = 3, byrow = TRUE)
  
  # 2) Cálculo de 11' (punto reflejado) sobre el plano definido por landmarks 23,24,25
  P11 <- coords[11, ]         # landmark 11
  B   <- coords[23, ]         # landmark 23
  C   <- coords[24, ]         # landmark 24
  D   <- coords[25, ]         # landmark 25 (proyección infradentale)
  
  # Producto vectorial para dos vectores 3D
  cross_product <- function(u, v) {
    c(
      u[2]*v[3] - u[3]*v[2],
      u[3]*v[1] - u[1]*v[3],
      u[1]*v[2] - u[2]*v[1]
    )
  }
  
  BC     <- C - B
  BD     <- D - B
  n      <- cross_product(BC, BD)
  n_unit <- n / sqrt(sum(n^2))
  d      <- sum((P11 - B) * n_unit)
  
  # Punto reflejado (11')
  P11_proj <- P11 - 2 * d * n_unit
  
  # Guardar en el workspace
  coords_11_prime <<- P11_proj
  dist_11_11prime <<- sqrt(sum((P11 - P11_proj)^2))
  
  # 3) Cálculo de las demás distancias
  dist_names <- c(
    "Mandibular lateral length",
    "Corpus length",
    "Dental arcade breadth",
    "Mandibular superior length",
    "Estimated bigonial breadth"
  )
  dist_vals <- c(
    sqrt(sum((coords[6, ]  - coords[13, ])^2)),
    sqrt(sum((coords[1, ]  - coords[11, ])^2)),
    sqrt(sum((coords[22, ] - coords[26, ])^2)),
    sqrt(sum((coords[23, ]  - coords[27, ])^2)),
    dist_11_11prime
  )
  
  # 4) Construcción de la matriz de salida
  result <- matrix(
    c(dist_names, as.character(dist_vals)),
    nrow    = 2,
    byrow   = TRUE,
    dimnames = list(c("Measurements", "Values"), NULL)
  )
  assign("distances_matrix", result, envir = .GlobalEnv)
  return(result)
}


#Extra - Calculate the euclidean distance in mm between two points
#' Calculate the distance between two 3D points
#'
#' @param point_A Numeric vector with three coordinates.
#' @param point_B Numeric vector with three coordinates.
#' @return Euclidean distance between the two points.
#' @export
PointDistancemm <- function(point_A, point_B) {
  distance <- sqrt(sum((point_A - point_B)^2))
  cat("The distance between point A and point B is:", distance, "\n")
}

# Extra - Transform text input into R vector function
#' Convert coordinate text to a numeric vector
#'
#' @param input_text Character string containing numeric values.
#' @return A numeric vector.
#' @export
text2vector <- function(input_text) {
  lines <- strsplit(input_text, "\n", fixed = TRUE)[[1]]
  values <- numeric(0)
  for (line in lines) {
    coords <- strsplit(line, " ")[[1]]
    coords <- as.numeric(trimws(coords))
    values <- c(values, coords)
  }
  return(values)
}

#Extra - Calculate the cross-product of two vector function
#' Compute the cross product between two vectors
#'
#' @param u_vector Numeric vector of length 3.
#' @param v_vector Numeric vector of length 3.
#' @return Numeric vector of length 3 with the cross product.
#' @export
cross_product <- function(u_vector, v_vector) {
  return(c(u[2]*v[3] - u[3]*v[2], u[3]*v[1] - u[1]*v[3], u[1]*v[2] - u[2]*v[1]))
}