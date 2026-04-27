# Internal numeric formatting helpers for command generation.
#
# These helpers are intentionally not exported. They standardize numeric
# formatting in software-specific command blocks, particularly Avizo TCL
# commands.

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