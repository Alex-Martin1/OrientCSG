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


# Internal formatting helper -------------------------------------------------
#
# Format numeric values for generated Python code. This uses compact decimal
# notation rather than fixed-width formatting so that direction vectors retain
# enough precision without making the generated block unnecessarily long.
fmt_num_py <- function(x, digits = 12) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    stop("`x` must be a single non-missing numeric value.", call. = FALSE)
  }

  formatC(as.numeric(x), digits = digits, format = "fg", flag = "#")
}

# Internal formatting helper -------------------------------------------------
#
# Format a numeric vector as a Python/numpy vector.
fmt_py_vec <- function(x, digits = 12) {
  x <- as.numeric(x)

  if (anyNA(x)) {
    stop("`x` must be a numeric vector without missing values.", call. = FALSE)
  }

  paste0(
    "np.array([",
    paste(vapply(x, fmt_num_py, character(1), digits = digits), collapse = ", "),
    "], dtype=float)"
  )
}

# Internal formatting helper -------------------------------------------------
#
# Quote a string for generated Python code using plain ASCII quotation marks.
# This intentionally avoids base::dQuote(), which may emit typographic quotes
# depending on locale/options and would break the generated Python block.
py_quote <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  sprintf('"%s"', x)
}
