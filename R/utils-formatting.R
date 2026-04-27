# Internal formatting helper -------------------------------------------------
#
# Format a single numeric value for command-line output. A fixed number of
# decimal places makes the generated Avizo TCL blocks easier to read, compare,
# and archive.
fmt_num <- function(x, digits = 6) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    stop("`x` must be a single non-missing numeric value.", call. = FALSE)
  }

  sprintf(paste0("%.", digits, "f"), x)
}

# Internal formatting helper -------------------------------------------------
#
# Format a numeric vector as a space-separated coordinate string. This is used
# when writing points, normals, and direction vectors into Avizo TCL commands.
fmt_vec <- function(x, digits = 6) {
  x <- as.numeric(x)

  if (anyNA(x)) {
    stop("`x` must be a numeric vector without missing values.", call. = FALSE)
  }

  paste(
    vapply(x, function(value) fmt_num(value, digits = digits), character(1)),
    collapse = " "
  )
}
