#' Get a TCL block from an orientation result
#'
#' @param res Result from `orient_mandible()` or `orient_longbone()`.
#' @param section Section name, e.g. "CS1", "CS2", "CS3", or "SECTION_35".
#' @return Character scalar.
#' @export
get_tcl <- function(res, section = NULL) {
  if (is.null(res$avizo_tcl)) stop("The object does not contain `avizo_tcl` blocks.", call. = FALSE)
  if (is.null(section)) {
    return(paste(unlist(res$avizo_tcl, use.names = FALSE), collapse = "\n\n# ------------------------------------------------------------\n\n"))
  }
  if (!section %in% names(res$avizo_tcl)) {
    stop(sprintf("Section '%s' does not exist. Available sections: %s", section, paste(names(res$avizo_tcl), collapse = ", ")), call. = FALSE)
  }
  res$avizo_tcl[[section]]
}

#' Copy an Avizo TCL block to the clipboard
#'
#' @param res Result from `orient_mandible()` or `orient_longbone()`.
#' @param section Section name. If `NULL`, copies all blocks.
#' @return Invisibly returns `TRUE` if copy was attempted successfully.
#' @export
copy_tcl <- function(res, section = NULL) {
  txt <- get_tcl(res, section = section)
  copy_to_clipboard(txt)
}

#' Write an Avizo TCL block to a file
#'
#' @param res Result from `orient_mandible()` or `orient_longbone()`.
#' @param file Output path.
#' @param section Section name. If `NULL`, writes all blocks.
#' @return Invisibly returns `file`.
#' @export
write_tcl <- function(res, file, section = NULL) {
  txt <- get_tcl(res, section = section)
  writeLines(txt, file)
  invisible(file)
}

copy_to_clipboard <- function(txt) {
  if (.Platform$OS.type == "windows") {
    utils::writeClipboard(txt)
    return(invisible(TRUE))
  }
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Darwin")) {
    con <- pipe("pbcopy", "w"); on.exit(close(con), add = TRUE)
    writeLines(txt, con)
    return(invisible(TRUE))
  }
  if (nzchar(Sys.which("xclip"))) {
    con <- pipe("xclip -selection clipboard", "w"); on.exit(close(con), add = TRUE)
    writeLines(txt, con)
    return(invisible(TRUE))
  }
  if (nzchar(Sys.which("xsel"))) {
    con <- pipe("xsel --clipboard --input", "w"); on.exit(close(con), add = TRUE)
    writeLines(txt, con)
    return(invisible(TRUE))
  }
  warning("The block could not be copied to the clipboard automatically.", call. = FALSE)
  invisible(FALSE)
}

#' Print a MandOri orientation object
#'
#' @param x A `mandori_orientation` object.
#' @param ... Additional arguments, ignored.
#' @export
print.mandori_orientation <- function(x, ...) {
  cat("MandOri orientation result\n")
  cat("Type:", x$type, "\n")
  cat("Individual:", x$individual_id, "\n")
  if (!is.null(x$avizo_tcl)) cat("TCL blocks:", paste(names(x$avizo_tcl), collapse = ", "), "\n")
  invisible(x)
}
