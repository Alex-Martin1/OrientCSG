#' Normalize a numeric vector
#'
#' @param v Numeric vector.
#' @return Unit vector.
#' @export
nrm <- function(v) {
  nv <- sqrt(sum(v * v))
  if (nv < 1e-15) stop("Norma ~0: no se puede normalizar el vector.", call. = FALSE)
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

#' Euclidean distance between two 3D points
#'
#' @param a,b Numeric vectors.
#' @return Numeric scalar.
#' @export
dist3 <- function(a, b) sqrt(sum((a - b)^2))

fmt_num <- function(x, digits = 6) {
  x <- as.numeric(x)
  x[abs(x) < 0.5 * 10^(-digits)] <- 0
  formatC(x, format = "f", digits = digits)
}

fmt_vec <- function(v, digits = 6) {
  paste(fmt_num(v, digits), collapse = " ")
}
