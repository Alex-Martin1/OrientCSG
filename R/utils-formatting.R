fmt_num <- function(x, digits = 6) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    stop("`x` must be a single non-missing numeric value.", call. = FALSE)
  }

  sprintf(paste0("%.", digits, "f"), x)
}

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


# Compact numeric representation for generated Python code.
fmt_num_py <- function(x, digits = 12) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    stop("`x` must be a single non-missing numeric value.", call. = FALSE)
  }

  formatC(as.numeric(x), digits = digits, format = "fg", flag = "#")
}

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

# Use ASCII quotes; dQuote() can emit invalid typographic Python quotes.
py_quote <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  sprintf('"%s"', x)
}
