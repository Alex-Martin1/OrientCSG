extract_nums <- function(txt) {
  clean <- gsub("[|,;]", " ", txt)
  scan(text = clean, quiet = TRUE)
}

# Parse numeric tokens from Slicer Markups rows; coordinate system is separate.
extract_numeric_tokens <- function(txt) {
  pattern <- "[-+]?(?:\\d*\\.\\d+|\\d+\\.?\\d*)(?:[eE][-+]?\\d+)?"
  matches <- gregexpr(pattern, txt, perl = TRUE)
  out <- regmatches(txt, matches)[[1]]
  as.numeric(out)
}

clean_landmark_lines <- function(txt) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]
  lines
}

# LPS <-> RAS: negate X and Y.
flip_xy <- function(x) {
  x <- as.numeric(x)
  c(-x[1], -x[2], x[3])
}

flip_xy_matrix <- function(mat) {
  out <- as.matrix(mat)
  out[, 1] <- -out[, 1]
  out[, 2] <- -out[, 2]
  colnames(out) <- colnames(mat)
  rownames(out) <- rownames(mat)
  out
}

resolve_lm_coord_system <- function(lm_coord_system = "LPS") {
  lm_coord_system <- toupper(trimws(lm_coord_system))
  if (length(lm_coord_system) != 1L || is.na(lm_coord_system) || !lm_coord_system %in% c("LPS", "RAS")) {
    stop('`lm_coord_system` must be "LPS" or "RAS".', call. = FALSE)
  }

  lm_coord_system
}

resolve_landmarks_str <- function(landmarks_str = NULL) {
  if (is.null(landmarks_str) || length(landmarks_str) != 1L || !nzchar(trimws(landmarks_str))) {
    stop("`landmarks_str` is required.", call. = FALSE)
  }

  landmarks_str
}

# Normalize landmarks to LPS. Slicer Markups exports may be LPS even when
# the interface displays RAS; honor lm_coord_system rather than guessing.
normalize_lm_coordinates <- function(coords, lm_coord_system = "LPS", arg = "lm_coord_system") {
  lm_coord_system <- toupper(trimws(lm_coord_system))

  if (length(lm_coord_system) != 1L || is.na(lm_coord_system) || !lm_coord_system %in% c("LPS", "RAS")) {
    stop(sprintf('`%s` must be "LPS" or "RAS".', arg), call. = FALSE)
  }

  out <- as.matrix(coords)
  if (ncol(out) != 3L) {
    stop("Landmark coordinates must be a matrix with three columns.", call. = FALSE)
  }

  if (lm_coord_system == "RAS") {
    out <- flip_xy_matrix(out)
  }

  colnames(out) <- c("x", "y", "z")
  rownames(out) <- rownames(coords)
  out
}

# Accept plain XYZ or Slicer Markups rows (XYZ in columns 2:4).
# Neither format determines the coordinate system or landmark order.
parse_landmarks <- function(landmarks_str, n_landmarks, context = "landmarks") {
  expected_n <- n_landmarks * 3

  lines <- clean_landmark_lines(landmarks_str)
  token_list <- lapply(lines, extract_numeric_tokens)
  token_lengths <- vapply(token_list, length, integer(1))

  detected_table <- length(lines) >= n_landmarks &&
    all(token_lengths[seq_len(n_landmarks)] >= 4L) &&
    any(token_lengths[seq_len(n_landmarks)] > 3L)

  detected_plain_lines <- length(lines) == n_landmarks &&
    all(token_lengths == 3L)

  if (detected_table) {
    if (length(lines) > n_landmarks) {
      warning(
        sprintf(
          "%d landmark rows were detected; only the first %d will be used.",
          length(lines), n_landmarks
        ),
        call. = FALSE
      )
    }

    mat <- do.call(
      rbind,
      lapply(token_list[seq_len(n_landmarks)], function(x) x[2:4])
    )
  } else if (detected_plain_lines) {
    mat <- do.call(rbind, token_list)
  } else {
    nums <- extract_numeric_tokens(landmarks_str)

    if (length(nums) < expected_n) {
      stop(
        sprintf(
          "%s requires %d numeric values: %d landmarks x 3 coordinates.",
          context, expected_n, n_landmarks
        ),
        call. = FALSE
      )
    }

    if (length(nums) > expected_n) {
      warning(
        sprintf(
          "%d numeric values were detected; only the first %d will be used.",
          length(nums), expected_n
        ),
        call. = FALSE
      )
    }

    mat <- matrix(nums[seq_len(expected_n)], ncol = 3, byrow = TRUE)
  }

  colnames(mat) <- c("x", "y", "z")

  mat
}

# BoneJ input: 3-vector, Log/3x3 matrix, or Results row (final nine values).
# Parse numeric fields rather than specimen IDs; axis 1 is longitudinal.
parse_bonej_eigenvectors <- function(longitudinal_matrix_str) {
  if (is.null(longitudinal_matrix_str) ||
      length(longitudinal_matrix_str) != 1L ||
      !nzchar(trimws(longitudinal_matrix_str))) {
    stop("`longitudinal_matrix_str` is required.", call. = FALSE)
  }

  clean <- gsub("[|,;]", " ", longitudinal_matrix_str)
  fields <- unlist(strsplit(trimws(clean), "\\s+", perl = TRUE), use.names = FALSE)
  nums <- suppressWarnings(as.numeric(fields))
  nums <- nums[is.finite(nums)]

  if (length(nums) == 3L) {
    return(matrix(nums, nrow = 3L, ncol = 1L))
  }

  if (length(nums) == 9L) {
    return(matrix(nums, nrow = 3L, byrow = TRUE))
  }

  if (length(nums) > 9L) {
    eig <- utils::tail(nums, 9L)
    return(cbind(
      eig[1:3],
      eig[4:6],
      eig[7:9]
    ))
  }

  stop(
    paste0(
      "`longitudinal_matrix_str` must contain either 3 numeric values ",
      "defining the BoneJ longitudinal vector, 9 numeric values defining ",
      "the current BoneJ Log/3 x 3 eigenvector matrix, or a full BoneJ Results-table ",
      "row containing the final nine unit-vector values."
    ),
    call. = FALSE
  )
}

# DICOM IOP (0020,0037): take the last six numeric fields when pasted.
parse_dicom_iop <- function(dicom_iop) {
  if (is.numeric(dicom_iop)) {
    if (length(dicom_iop) != 6L || any(!is.finite(dicom_iop))) {
      stop("`dicom_iop` must contain six finite numeric values.", call. = FALSE)
    }
    return(as.numeric(dicom_iop))
  }

  if (!is.character(dicom_iop) || length(dicom_iop) != 1L || !nzchar(trimws(dicom_iop))) {
    stop(
      "`dicom_iop` must be a numeric vector of length 6 or a pasted DICOM Image Orientation (Patient) line.",
      call. = FALSE
    )
  }

  nums <- extract_numeric_tokens(dicom_iop)
  if (length(nums) >= 6L) nums <- utils::tail(nums, 6L)

  if (length(nums) != 6L || any(!is.finite(nums))) {
    stop("Could not parse six finite values from `dicom_iop`.", call. = FALSE)
  }

  as.numeric(nums)
}

# DICOM IPP (0020,0032); consecutive entries must follow BoneJ stack order.
parse_dicom_ipp <- function(dicom_ipp, arg_name = "dicom_ipp") {
  if (is.numeric(dicom_ipp)) {
    if (length(dicom_ipp) != 3L || any(!is.finite(dicom_ipp))) {
      stop(sprintf("`%s` must contain three finite numeric values.", arg_name), call. = FALSE)
    }
    return(as.numeric(dicom_ipp))
  }

  if (!is.character(dicom_ipp) || length(dicom_ipp) != 1L || !nzchar(trimws(dicom_ipp))) {
    stop(
      sprintf("`%s` must be a numeric vector of length 3 or a pasted DICOM Image Position (Patient) line.", arg_name),
      call. = FALSE
    )
  }

  nums <- extract_numeric_tokens(dicom_ipp)
  if (length(nums) >= 3L) nums <- utils::tail(nums, 3L)

  if (length(nums) != 3L || any(!is.finite(nums))) {
    stop(sprintf("Could not parse three finite values from `%s`.", arg_name), call. = FALSE)
  }

  as.numeric(nums)
}

# Parse the combined volume-input DICOM orientation input. The public
# `dicom_orientation` argument is normally a three-element character vector
# constructed as c(IOP, IPP1, IPP2). A single three-line character block is
# also accepted. The IOP line is identified by tag/name when possible; the two
# IPP lines retain their supplied order because that order defines stack Z.
parse_dicom_orientation <- function(dicom_orientation) {
  if (is.numeric(dicom_orientation)) {
    if (length(dicom_orientation) != 12L || any(!is.finite(dicom_orientation))) {
      stop(
        "Numeric `dicom_orientation` input must contain 12 finite values: 6 IOP values followed by 3 IPP1 and 3 IPP2 values.",
        call. = FALSE
      )
    }
    return(list(
      iop = as.numeric(dicom_orientation[1:6]),
      ipp_1 = as.numeric(dicom_orientation[7:9]),
      ipp_2 = as.numeric(dicom_orientation[10:12])
    ))
  }

  if (!is.character(dicom_orientation) || length(dicom_orientation) < 1L) {
    stop(
      "`dicom_orientation` must contain one DICOM Image Orientation (Patient) line and two consecutive Image Position (Patient) lines.",
      call. = FALSE
    )
  }

  lines <- unlist(strsplit(paste(dicom_orientation, collapse = "\n"), "[\r\n]+", perl = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (length(lines) != 3L) {
    stop(
      "`dicom_orientation` must contain exactly three non-empty entries: one IOP line and two consecutive IPP lines.",
      call. = FALSE
    )
  }

  is_iop <- grepl("0020\\s*,\\s*0037", lines, ignore.case = TRUE, perl = TRUE) |
    grepl("Image Orientation \\(Patient\\)", lines, ignore.case = TRUE, perl = TRUE)
  is_ipp <- grepl("0020\\s*,\\s*0032", lines, ignore.case = TRUE, perl = TRUE) |
    grepl("Image Position \\(Patient\\)", lines, ignore.case = TRUE, perl = TRUE)

  if (sum(is_iop) == 1L && sum(is_ipp) == 2L) {
    iop_line <- lines[is_iop]
    ipp_lines <- lines[is_ipp]
  } else {
    # Fallback for compact untagged text: IOP first, then IPP1 and IPP2.
    iop_line <- lines[1]
    ipp_lines <- lines[2:3]
  }

  list(
    iop = parse_dicom_iop(iop_line),
    ipp_1 = parse_dicom_ipp(ipp_lines[1], "dicom_orientation IPP1"),
    ipp_2 = parse_dicom_ipp(ipp_lines[2], "dicom_orientation IPP2")
  )
}

# Format parsed DICOM values compactly for the result summary while retaining
# sufficient numeric precision for traceability.
format_dicom_values <- function(x) {
  paste(vapply(as.numeric(x), function(v) {
    format(v, digits = 15, scientific = FALSE, trim = TRUE)
  }, character(1)), collapse = "\\")
}

# Build the transformation from the ImageJ/BoneJ stack basis to the internal
# DICOM/LPS patient basis. IOP defines the two in-plane stack axes. Their cross
# product defines the slice normal up to sign; two consecutive IPP positions,
# supplied in BoneJ stack order, determine that sign. This avoids assuming that
# stack index Z necessarily increases in the IOP cross-product direction.
dicom_orientation_to_bonej_transform <- function(dicom_orientation,
                                                  alignment_tolerance = 0.999) {
  orientation <- parse_dicom_orientation(dicom_orientation)
  iop <- orientation$iop
  ipp_1 <- orientation$ipp_1
  ipp_2 <- orientation$ipp_2

  row_axis <- nrm(iop[1:3])
  col_axis <- nrm(iop[4:6])

  dot_rc <- dot3(row_axis, col_axis)
  if (abs(dot_rc) > 1e-4) {
    warning(
      "The DICOM row and column direction cosines are not orthogonal; the column axis was re-orthogonalized.",
      call. = FALSE
    )
  }

  col_axis <- col_axis - dot3(col_axis, row_axis) * row_axis
  if (sqrt(sum(col_axis^2)) < 1e-12) {
    stop("The DICOM row and column direction cosines are collinear.", call. = FALSE)
  }
  col_axis <- nrm(col_axis)

  iop_normal <- nrm(cross3(row_axis, col_axis))
  ipp_delta <- ipp_2 - ipp_1
  slice_spacing <- sqrt(sum(ipp_delta^2))
  if (!is.finite(slice_spacing) || slice_spacing < 1e-12) {
    stop(
      "The two IPP entries in `dicom_orientation` must refer to two distinct consecutive slices in BoneJ stack order.",
      call. = FALSE
    )
  }

  ipp_direction <- ipp_delta / slice_spacing
  signed_alignment <- dot3(ipp_direction, iop_normal)
  alignment <- abs(signed_alignment)
  if (!is.finite(alignment) || alignment < alignment_tolerance) {
    stop(
      paste0(
        "The displacement between the two IPP entries in `dicom_orientation` is not parallel to the DICOM image-plane normal. ",
        "Use Image Position (Patient) values from two consecutive slices, in the exact order used in the BoneJ stack, ",
        "and do not use a stack that was reoriented or resliced after DICOM import."
      ),
      call. = FALSE
    )
  }

  slice_direction <- if (signed_alignment < 0) -1 else 1
  slice_axis <- slice_direction * iop_normal

  out <- cbind(row_axis, col_axis, slice_axis)
  rownames(out) <- c("x", "y", "z")
  colnames(out) <- c("stack_x", "stack_y", "stack_z")

  attr(out, "dicom_iop") <- iop
  attr(out, "dicom_ipp_1") <- ipp_1
  attr(out, "dicom_ipp_2") <- ipp_2
  attr(out, "iop_normal") <- iop_normal
  attr(out, "slice_direction") <- slice_direction
  attr(out, "slice_spacing") <- slice_spacing
  attr(out, "slice_alignment") <- alignment
  out
}
