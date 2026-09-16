# Internal DICOM metadata reader ----------------------------------------------
#
# Fast TRUE-volume helper used by orient_longbone(). It opens only enough files
# to obtain two readable DICOM headers, never loads pixel data, orders the pair
# by InstanceNumber, and validates the IOP/IPP geometry required by the existing
# BoneJ-to-LPS transform. This helper is deliberately not part of the public API.

.read_dicom_orientation <- function(dicom_dir, recursive = TRUE, verbose = FALSE) {
  if (!is.character(dicom_dir) || length(dicom_dir) != 1L ||
      is.na(dicom_dir) || !nzchar(trimws(dicom_dir))) {
    stop("`dicom_dir` must be a single non-empty character path.", call. = FALSE)
  }
  if (!dir.exists(dicom_dir)) {
    stop(sprintf("DICOM directory does not exist: %s", dicom_dir), call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!requireNamespace("oro.dicom", quietly = TRUE)) {
    stop(
      paste0(
        "Automatic DICOM metadata reading requires the suggested package `oro.dicom`. ",
        "Install it with install.packages(\"oro.dicom\")."
      ),
      call. = FALSE
    )
  }

  files <- list.files(
    dicom_dir,
    full.names = TRUE,
    recursive = isTRUE(recursive),
    all.files = FALSE,
    include.dirs = FALSE
  )
  if (length(files) == 0L) {
    stop("No files were found in `dicom_dir`.", call. = FALSE)
  }

  if (length(files) < 2L) {
    stop("Fewer than two files were found in `dicom_dir`.", call. = FALSE)
  }

  candidate_count <- length(files)
  headers <- list()
  selected_files <- character(0)

  for (file in files) {
    parsed <- tryCatch(
      oro.dicom::readDICOMFile(
        file,
        pixelData = FALSE,
        skipSequence = TRUE,
        warn = -1
      ),
      error = function(e) NULL
    )

    if (!is.null(parsed) && is.data.frame(parsed$hdr)) {
      headers[[length(headers) + 1L]] <- parsed$hdr
      selected_files <- c(selected_files, file)
    }

    if (length(headers) == 2L) break
  }

  if (length(headers) < 2L) {
    stop(
      "Fewer than two readable DICOM headers were found in `dicom_dir`.",
      call. = FALSE
    )
  }

  out <- .dicom_orientation_from_headers(
    headers,
    files = selected_files
  )

  if (abs(out$instance_2 - out$instance_1) != 1) {
    stop(
      paste0(
        "The first two readable DICOM files are not consecutive by ",
        "InstanceNumber (0020,0013): ", out$instance_1, " and ",
        out$instance_2, ". OrientCSG intentionally reads only the first ",
        "two slices for speed. Use a directory whose file order follows the ",
        "slice sequence, or supply `dicom_orientation` manually."
      ),
      call. = FALSE
    )
  }

  out$dicom_dir <- normalizePath(dicom_dir, winslash = "/", mustWork = TRUE)
  out$n_files_found <- candidate_count
  out$headers_read <- 2L

  if (isTRUE(verbose)) {
    message("DICOM metadata fast path: read 2 header(s) only.")
    message(sprintf("Candidate files in directory: %d", candidate_count))
    message(sprintf("DICOM series: %s", out$series_uid))
    message(sprintf(
      "Slices used: InstanceNumber %s -> %s",
      out$instance_1,
      out$instance_2
    ))
    message(sprintf("Slice spacing: %.9g mm", out$slice_spacing))
    message(sprintf("File 1: %s", out$file_1))
    message(sprintf("File 2: %s", out$file_2))
  }

  out
}


.dicom_header_tag_value <- function(hdr, group, element, name = NULL) {
  if (!is.data.frame(hdr) || nrow(hdr) == 0L) return(NA_character_)

  idx <- integer(0)
  if (all(c("group", "element") %in% names(hdr))) {
    g <- toupper(trimws(as.character(hdr$group)))
    e <- toupper(trimws(as.character(hdr$element)))
    idx <- which(g == toupper(group) & e == toupper(element))
  }
  if (length(idx) == 0L && !is.null(name) && "name" %in% names(hdr)) {
    idx <- which(as.character(hdr$name) == name)
  }
  if (length(idx) == 0L || !"value" %in% names(hdr)) return(NA_character_)

  value <- as.character(hdr$value[idx[1L]])
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) NA_character_ else value
}


.dicom_orientation_from_headers <- function(headers, files = names(headers), iop_tolerance = 1e-6,
                                            alignment_tolerance = 0.999) {
  if (!is.list(headers) || length(headers) < 2L) {
    stop("At least two DICOM headers are required.", call. = FALSE)
  }
  if (is.null(files) || length(files) != length(headers)) {
    files <- paste0("slice_", seq_along(headers))
  }

  rows <- lapply(seq_along(headers), function(i) {
    hdr <- headers[[i]]
    list(
      file = as.character(files[i]),
      series_uid = .dicom_header_tag_value(hdr, "0020", "000E", "SeriesInstanceUID"),
      instance = .dicom_header_tag_value(hdr, "0020", "0013", "InstanceNumber"),
      ipp = .dicom_header_tag_value(hdr, "0020", "0032", "ImagePositionPatient"),
      iop = .dicom_header_tag_value(hdr, "0020", "0037", "ImageOrientationPatient")
    )
  })

  eligible <- vapply(rows, function(x) {
    all(vapply(x[c("instance", "ipp", "iop")], function(v) !is.na(v) && nzchar(trimws(v)), logical(1)))
  }, logical(1))
  rows <- rows[eligible]
  if (length(rows) < 2L) {
    stop(
      "Fewer than two DICOM slices contain InstanceNumber, ImagePositionPatient, and ImageOrientationPatient.",
      call. = FALSE
    )
  }

  series <- vapply(rows, `[[`, character(1), "series_uid")
  known_series <- unique(series[!is.na(series) & nzchar(trimws(series))])
  if (length(known_series) > 1L) {
    stop(
      paste0(
        "Multiple DICOM SeriesInstanceUID values were found in `dicom_dir`: ",
        paste(known_series, collapse = ", "),
        ". Use a directory containing only the BoneJ CT series."
      ),
      call. = FALSE
    )
  }
  if (length(known_series) == 1L) {
    rows <- rows[!is.na(series) & nzchar(trimws(series)) & series == known_series]
    series_uid <- known_series
  } else {
    series_uid <- "<not recorded>"
  }
  if (length(rows) < 2L) {
    stop("The selected DICOM series contains fewer than two usable slices.", call. = FALSE)
  }

  instance <- suppressWarnings(as.numeric(vapply(rows, `[[`, character(1), "instance")))
  if (any(!is.finite(instance))) {
    stop(
      "Automatic DICOM ordering requires a finite InstanceNumber (0020,0013) for every selected slice.",
      call. = FALSE
    )
  }
  if (anyDuplicated(instance)) {
    stop(
      "Automatic DICOM ordering requires unique InstanceNumber (0020,0013) values within the selected series.",
      call. = FALSE
    )
  }

  ord <- order(instance)
  rows <- rows[ord]
  instance <- instance[ord]

  iop_list <- lapply(rows, function(x) parse_dicom_iop(x$iop))
  iop_mat <- do.call(rbind, iop_list)
  ref_iop <- iop_mat[1L, ]
  if (any(abs(sweep(iop_mat, 2L, ref_iop, FUN = "-")) > iop_tolerance)) {
    stop("Image Orientation (Patient) is not consistent across the selected DICOM series.", call. = FALSE)
  }

  ipp_list <- lapply(rows, function(x) parse_dicom_ipp(x$ipp, "DICOM ImagePositionPatient"))
  ipp_mat <- do.call(rbind, ipp_list)

  row_axis <- nrm(ref_iop[1:3])
  col_axis <- ref_iop[4:6] - dot3(ref_iop[4:6], row_axis) * row_axis
  if (sqrt(sum(col_axis^2)) < 1e-12) {
    stop("The DICOM row and column direction cosines are collinear.", call. = FALSE)
  }
  col_axis <- nrm(col_axis)
  normal <- nrm(cross3(row_axis, col_axis))

  deltas <- ipp_mat[-1L, , drop = FALSE] - ipp_mat[-nrow(ipp_mat), , drop = FALSE]
  spacings <- sqrt(rowSums(deltas^2))
  if (any(!is.finite(spacings)) || any(spacings < 1e-12)) {
    stop("Adjacent DICOM slices contain duplicate or invalid Image Position (Patient) values.", call. = FALSE)
  }
  alignments_signed <- as.numeric(deltas %*% normal) / spacings
  if (any(abs(alignments_signed) < alignment_tolerance)) {
    stop(
      "InstanceNumber ordering is not parallel to the DICOM image-plane normal; automatic BoneJ stack orientation cannot be guaranteed.",
      call. = FALSE
    )
  }
  nonzero_signs <- sign(alignments_signed)
  if (length(unique(nonzero_signs)) != 1L) {
    stop(
      "InstanceNumber ordering changes physical slice direction within the selected DICOM series; automatic BoneJ stack orientation cannot be guaranteed.",
      call. = FALSE
    )
  }

  iop <- ref_iop
  ipp_1 <- ipp_mat[1L, ]
  ipp_2 <- ipp_mat[2L, ]

  iop_line <- sprintf(
    "(0020,0037  Image Orientation (Patient): %s)",
    format_dicom_values(iop)
  )
  ipp_1_line <- sprintf(
    "(0020,0032  Image Position (Patient): %s)",
    format_dicom_values(ipp_1)
  )
  ipp_2_line <- sprintf(
    "(0020,0032  Image Position (Patient): %s)",
    format_dicom_values(ipp_2)
  )

  list(
    dicom_orientation = c(iop_line, ipp_1_line, ipp_2_line),
    iop = iop,
    ipp_1 = ipp_1,
    ipp_2 = ipp_2,
    series_uid = series_uid,
    n_slices = length(rows),
    order_method = "InstanceNumber ascending",
    instance_1 = instance[1L],
    instance_2 = instance[2L],
    file_1 = rows[[1L]]$file,
    file_2 = rows[[2L]]$file,
    slice_spacing = sqrt(sum((ipp_2 - ipp_1)^2))
  )
}
