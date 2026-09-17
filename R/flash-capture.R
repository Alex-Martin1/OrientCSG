# Batch long-bone section capture --------------------------------------------
#
# Internal generators live in this file because Flash Capture is deliberately
# separated from the orientation backends. orient_longbone() establishes the
# geometry; flash_capture() either initializes the Avizo/Amira view from that
# geometry or reuses a prepared 3D Slicer reference view to export multiple
# requested sections.

#' Batch-capture long-bone cross-sections
#'
#' `flash_capture()` generates a single batch command block that captures multiple
#' long-bone sections from an existing [orient_longbone()] result. The function
#' supports the three primary OrientCSG capture workflows: Avizo/Amira with a CT
#' volume (`SLICER = FALSE`, `SOLID = FALSE`), 3D Slicer with a CT volume
#' (`SLICER = TRUE`, `SOLID = FALSE`), and 3D Slicer with a solid surface mesh
#' (`SLICER = TRUE`, `SOLID = TRUE`).
#'
#' In 3D Slicer, Flash Capture deliberately does not recompute anatomical
#' orientation: first run one reference section with [copy_slicer_py()] and
#' configure the desired view; Flash Capture preserves that prepared view across
#' the batch. In Avizo/Amira, no prior [copy_tcl()] step is required: Flash
#' Capture initializes the first requested section with the same Slice and AP/ML
#' orientation used by [copy_tcl()] and rotates the existing camera so it is
#' perpendicular to the section. The current camera position, projection type,
#' and zoom/framing are preserved, and that oriented camera is then reused while
#' the Slice moves through the remaining requested sections. Brightness/contrast,
#' colormap, and scale settings are also left unchanged. In the
#' 3D Slicer CT workflow, the current slice matrix and field of
#' view are used as the visual reference while the section plane and OrientCSG
#' scale are translated between requested section points. In the 3D Slicer SOLID
#' workflow, the source mesh is re-cut at each requested section while the current
#' 3D camera orientation, proximal viewing side, parallel scale, and relative pan
#' are preserved. The starting Slicer view is restored after batch capture.
#'
#' Flash Capture writes TIFF files in RGB mode by default. In 3D Slicer,
#' `color_mode = "grayscale"` converts the rendered RGB buffer to a true
#' single-channel luminance image before TIFF writing and uses lossless Deflate
#' compression. Avizo/Amira scripted snapshots do not expose a reliable
#' grayscale switch, so requesting grayscale there produces a warning and falls
#' back to the native RGB snapshot. Avizo/Amira Flash Capture always uses viewer
#' 0. Slicer reference-section detection uses a fixed 2 mm tolerance; these
#' implementation constants are intentionally not public arguments.
#'
#' @param res An `orientcsg_longbone` result returned by [orient_longbone()].
#' @param output_dir Directory in which the external application should save the
#'   captured images. The directory is created by the generated TCL/Python block
#'   if needed.
#' @param file_name Base name for the captured files. If `NULL`,
#'   `res$individual_id` is used.
#' @param sections Optional section selection. Accepts numeric percentages such
#'   as `c(20, 50, 80)` or names such as `c("SECTION_20", "SECTION_50")`.
#'   If `NULL`, all section points in `res` are captured.
#' @param extension Output file extension. Slicer Flash Capture requires `"tif"`
#'   or `"tiff"`; Avizo/Amira retains its native snapshot extension support.
#' @param slice_view Name of the Slicer slice view used by the CT-volume branch;
#'   defaults to `"Red"`.
#' @param color_mode Output color mode. `"rgb"` is the default. In 3D
#'   Slicer, `"grayscale"` writes a true single-channel TIFF. In Avizo/Amira,
#'   grayscale export is not reliably scriptable, so `"grayscale"` falls back
#'   to the native RGB snapshot with a warning.
#' @param copy Logical. If `TRUE`, copy the generated TCL/Python block to the
#'   system clipboard using the same clipboard helper as [copy_tcl()] and
#'   [copy_slicer_py()]. If `FALSE`, only return the generated block invisibly.
#'
#' @return Invisibly returns a character scalar containing the generated batch
#'   command block.
#'
#' @examples
#' \dontrun{
#' # First prepare a reference view, for example SECTION_50:
#' copy_slicer_py(res, section = "SECTION_50")
#'
#' # After configuring that view in Slicer, generate one batch block:
#' flash_capture(
#'   res,
#'   output_dir = "C:/OrientCSG/captures",
#'   sections = c(20, 35, 50, 65, 80)
#' )
#' }
#'
#' @export
flash_capture <- function(
    res,
    output_dir,
    file_name = NULL,
    sections = NULL,
    extension = "tif",
    slice_view = "Red",
    color_mode = c("rgb", "grayscale"),
    copy = TRUE
) {
  if (!inherits(res, "orientcsg_longbone")) {
    stop("`res` must be an OrientCSG long-bone result.", call. = FALSE)
  }

  output_dir <- .flash_capture_validate_scalar_string(output_dir, "output_dir")

  if (is.null(file_name)) {
    file_name <- res$individual_id
  }
  file_name <- .flash_capture_validate_scalar_string(file_name, "file_name")

  if (is.null(res$section_points) || length(res$section_points) == 0L ||
      is.null(names(res$section_points))) {
    stop("`res` does not contain valid named section points.", call. = FALSE)
  }

  extension <- sub("^\\.", "", .flash_capture_validate_scalar_string(extension, "extension"))
  slice_view <- .flash_capture_validate_scalar_string(slice_view, "slice_view")
  color_mode <- match.arg(color_mode)

  if (isTRUE(res$SLICER) && !tolower(extension) %in% c("tif", "tiff")) {
    stop("Slicer Flash Capture currently supports TIFF output only.", call. = FALSE)
  }

  if (!isTRUE(res$SLICER) && identical(color_mode, "grayscale")) {
    warning(
      paste0(
        "Avizo/Amira `viewer snapshot` does not expose a reliable scripted ",
        "grayscale mode; using the native RGB snapshot instead. Convert the ",
        "captured TIFF to grayscale afterwards if needed."
      ),
      call. = FALSE
    )
    color_mode <- "rgb"
  }

  if (isTRUE(res$SLICER)) {
    if (isTRUE(res$SOLID)) {
      return(.flash_capture_slicer_solid(
        res = res,
        file_name = file_name,
        output_dir = output_dir,
        sections = sections,
        color_mode = color_mode,
        copy = copy
      ))
    }

    return(.flash_capture_slicer_ct(
      res = res,
      file_name = file_name,
      output_dir = output_dir,
      sections = sections,
      slice_view = slice_view,
      color_mode = color_mode,
      copy = copy
    ))
  }

  .flash_capture_avizo(
    res = res,
    file_name = file_name,
    output_dir = output_dir,
    sections = sections,
    extension = extension,
    copy = copy
  )
}



.flash_capture_select_sections <- function(available, sections = NULL) {
  if (is.null(available) || length(available) == 0L) {
    stop("No named sections are available in `res$section_points`.", call. = FALSE)
  }

  if (is.null(sections)) {
    return(available)
  }

  if (is.numeric(sections)) {
    selected <- paste0("SECTION_", sections)
  } else {
    sections <- as.character(sections)
    selected <- ifelse(
      grepl("^SECTION_", sections),
      sections,
      paste0("SECTION_", sections)
    )
  }

  selected <- unique(selected)
  missing <- setdiff(selected, available)
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Unknown section(s): %s. Available sections: %s",
        paste(missing, collapse = ", "),
        paste(available, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  selected
}

.flash_capture_validate_scalar_string <- function(x, name) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a non-empty character string.", name), call. = FALSE)
  }
  x
}



.flash_capture_avizo <- function(
    res,
    file_name,
    output_dir,
    sections = NULL,
    extension = "tif",
    copy = TRUE
) {
  if (is.null(res$section_points) || length(res$section_points) == 0L) {
    stop("`res` does not contain section points.", call. = FALSE)
  }
  if (is.null(res$vectors$L) || length(res$vectors$L) != 3L) {
    stop("`res` does not contain a valid longitudinal vector.", call. = FALSE)
  }
  if (isTRUE(res$SLICER)) {
    stop("The Avizo/Amira Flash Capture branch requires `SLICER = FALSE`.", call. = FALSE)
  }
  if (isTRUE(res$SOLID)) {
    stop(
      "Avizo/Amira Flash Capture is currently implemented only for `SOLID = FALSE` long-bone results.",
      call. = FALSE
    )
  }

  # Fixed implementation detail: Flash Capture uses the main viewer.
  viewer_id <- 0L

  extension <- sub("^\\.", "", extension)
  if (!nzchar(extension)) {
    stop("`extension` must contain a file extension.", call. = FALSE)
  }

  output_dir <- chartr("\\", "/", output_dir)
  if (grepl("\\}", output_dir) || grepl("\\}", file_name)) {
    stop("Avizo/Amira Flash Capture paths and file names cannot contain '}'.", call. = FALSE)
  }

  available <- names(res$section_points)
  selected <- .flash_capture_select_sections(available, sections)

  fmt <- function(x) {
    formatC(as.numeric(x), format = "f", digits = 6, drop0trailing = FALSE)
  }
  fmt_vec <- function(x) paste(fmt(x), collapse = " ")

  L <- res$vectors$L
  if (isTRUE(res$USE_ANAT_ORIENT)) {
    ML <- res$vectors$ML
    AP <- res$vectors$AP
  } else {
    ML <- res$vectors$X_screen
    AP <- res$vectors$Y_screen
  }
  if (is.null(ML) || length(ML) != 3L || is.null(AP) || length(AP) != 3L) {
    stop("`res` does not contain valid transverse orientation vectors.", call. = FALSE)
  }

  reference_section <- selected[[1L]]
  P_ref <- res$section_points[[reference_section]]
  ML_normal <- nrm(cross3(L, ML))
  AP_normal <- nrm(cross3(L, AP))

  initial_orientation <- c(
    "# ------------------------------------------------------------",
    sprintf("# INITIAL ORIENTATION FROM %s", reference_section),
    "# ------------------------------------------------------------",
    "# Apply the current result's section orientation before capture.",
    "# Rotate the camera once while preserving its position and zoom.",
    "",
    emit_normal_point_plane("Slice", P_ref, L)
  )

  if (isTRUE(res$USE_ANAT_ORIENT)) {
    initial_orientation <- c(
      initial_orientation,
      "",
      "# ML visual plane",
      emit_normal_point_plane(
        "ML", P_ref, ML_normal, color = c(0, 1, 0), hide_points = TRUE
      ),
      "",
      "# AP visual plane",
      emit_normal_point_plane(
        "AP", P_ref, AP_normal, color = c(0, 0, 1), hide_points = TRUE
      )
    )
  }

  initial_orientation <- c(
    initial_orientation,
    "",
    emit_longbone_camera(
      P_ref, L, ML, AP,
      mode = if (isTRUE(res$USE_ANAT_ORIENT)) res$type else "SECTION_ONLY",
      orientation_only = TRUE
    ),
    ""
  )

  out <- c(
    "# ============================================================",
    "# OrientCSG FLASH CAPTURE",
    "# Avizo/Amira / CT volume",
    "# ============================================================",
    "#",
    "# The first requested section initializes Slice orientation,",
    "# AP/ML visual planes (when available), and camera orientation.",
    "# Camera position, projection type, and zoom/framing are preserved.",
    "# The oriented camera is then reused while Slice moves through",
    "# the remaining requested sections.",
    "# Brightness/contrast, colormap, and scale settings are unchanged.",
    "# ============================================================",
    "",
    sprintf("set OrientCSG_output_dir {%s}", output_dir),
    "file mkdir $OrientCSG_output_dir",
    "",
    initial_orientation
  )

  for (sec in selected) {
    P <- res$section_points[[sec]]
    pct <- sub("^SECTION_", "", sec)
    output_name <- paste0(file_name, "_", pct, ".", extension)

    out <- c(
      out,
      "# ------------------------------------------------------------",
      sprintf("# SECTION %s%%", pct),
      "# ------------------------------------------------------------",
      "",
      "\"Slice\" planeDefinition setValue 0",
      sprintf("\"Slice\" origin setCoord 0 %s", fmt_vec(P)),
      sprintf("\"Slice\" normal setCoord 0 %s", fmt_vec(L)),
      "catch {\"Slice\" origin showPoints 0}",
      "catch {\"Slice\" point showPoints 0}",
      "catch {\"Slice\" planePoint1 showPoints 0}",
      "catch {\"Slice\" planePoint2 showPoints 0}",
      "catch {\"Slice\" planePoint3 showPoints 0}",
      "\"Slice\" fire",
      "",
      sprintf("viewer %d redraw", viewer_id),
      sprintf("set OrientCSG_file [file join $OrientCSG_output_dir {%s}]", output_name),
      sprintf("viewer %d snapshot $OrientCSG_file", viewer_id),
      "echo \"Saved: $OrientCSG_file\"",
      ""
    )
  }

  out <- c(
    out,
    "# ============================================================",
    "echo \"OrientCSG flash capture finished.\"",
    "echo \"Output folder: $OrientCSG_output_dir\"",
    "# ============================================================"
  )

  txt <- paste(out, collapse = "\n")

  if (isTRUE(copy)) {
    copied <- copy_to_clipboard(txt)
    if (isTRUE(copied)) {
      message(sprintf(
        "Avizo/Amira Flash Capture TCL copied to clipboard (%d sections).",
        length(selected)
      ))
    }
  }

  invisible(txt)
}


.flash_capture_slicer_ct <- function(
    res,
    file_name,
    output_dir,
    sections = NULL,
    slice_view = "Red",
    color_mode = c("rgb", "grayscale"),
    copy = TRUE
) {

  reference_tolerance_mm <- 2
  color_mode <- match.arg(color_mode)

  # ----------------------------------------------------------
  # CHECK RESULT TYPE / WORKFLOW
  # ----------------------------------------------------------

  if (!inherits(res, "orientcsg_longbone")) {
    stop(
      "`res` must be an OrientCSG long-bone result.",
      call. = FALSE
    )
  }

  if (!isTRUE(res$SLICER)) {
    stop(
      "Slicer CT Flash Capture requires `SLICER = TRUE`.",
      call. = FALSE
    )
  }

  if (isTRUE(res$SOLID)) {
    stop(
      "This Flash Capture branch is for Slicer CT volumes (`SOLID = FALSE`) only.",
      call. = FALSE
    )
  }

  if (is.null(res$section_points) ||
      length(res$section_points) == 0L) {
    stop(
      "`res` does not contain section points.",
      call. = FALSE
    )
  }

  if (is.null(names(res$section_points))) {
    stop(
      "`res$section_points` must be named.",
      call. = FALSE
    )
  }


  # ----------------------------------------------------------
  # CHECK FILE NAME / OUTPUT DIRECTORY
  # ----------------------------------------------------------

  if (length(file_name) != 1L ||
      is.na(file_name) ||
      !nzchar(file_name)) {
    stop(
      "`file_name` must be a non-empty character string.",
      call. = FALSE
    )
  }

  if (length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(output_dir)) {
    stop(
      "`output_dir` must be a non-empty character string.",
      call. = FALSE
    )
  }

  # Use forward slashes for the Python/Slicer path
  output_dir <- chartr("\\", "/", output_dir)


  # ----------------------------------------------------------
  # SELECT SECTIONS
  # ----------------------------------------------------------

  available <- names(res$section_points)

  if (is.null(sections)) {

    selected <- available

  } else {

    if (is.numeric(sections)) {

      selected <- paste0("SECTION_", sections)

    } else {

      sections <- as.character(sections)

      selected <- ifelse(
        grepl("^SECTION_", sections),
        sections,
        paste0("SECTION_", sections)
      )
    }

    missing <- setdiff(selected, available)

    if (length(missing) > 0L) {

      stop(
        sprintf(
          paste0(
            "Unknown section(s): %s.\n",
            "Available sections: %s"
          ),
          paste(missing, collapse = ", "),
          paste(available, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  selected <- unique(selected)


  # ----------------------------------------------------------
  # CONVERT INTERNAL LPS POINTS TO SLICER RAS
  # ----------------------------------------------------------

  flip_xy <- function(v) {

    v <- as.numeric(v)

    c(
      -v[1],
      -v[2],
      v[3]
    )
  }

  points_ras <- lapply(
    res$section_points,
    flip_xy
  )


  # ----------------------------------------------------------
  # FORMATTERS
  # ----------------------------------------------------------

  fmt_num <- function(x) {

    formatC(
      as.numeric(x),
      format = "f",
      digits = 9,
      drop0trailing = FALSE
    )
  }

  py_vec <- function(v) {

    paste0(
      "np.array([",
      paste(fmt_num(v), collapse = ", "),
      "], dtype=float)"
    )
  }

  py_string <- function(x) {

    encodeString(
      as.character(x),
      quote = "'"
    )
  }


  # ----------------------------------------------------------
  # PYTHON DICTIONARY WITH ALL SECTION POINTS
  # ----------------------------------------------------------

  section_lines <- vapply(
    names(points_ras),
    function(sec) {

      paste0(
        "    ",
        py_string(sec),
        ": ",
        py_vec(points_ras[[sec]]),
        ","
      )
    },
    character(1)
  )


  capture_lines <- paste0(
    "    ",
    vapply(selected, py_string, character(1)),
    ","
  )


  # ----------------------------------------------------------
  # BUILD SLICER PYTHON BLOCK
  # ----------------------------------------------------------

  code <- c(

    "import os",
    "import slicer",
    "import vtk",
    "import numpy as np",
    "",

    "# ============================================================",
    "# OrientCSG FLASH CAPTURE",
    "# 3D Slicer / CT volume",
    "# ============================================================",
    "#",
    "# The CURRENT Red slice view is used as the visual reference.",
    "#",
    "# Preserved:",
    "#   - slice orientation",
    "#   - zoom / field of view",
    "#   - in-plane pan",
    "#   - brightness / contrast / window-level",
    "#   - current layout and Red-view pixel size",
    "#",
    "# OrientCSG scale markups are translated with each section.",
    "#",
    "# The script restores the starting Red slice view afterwards.",
    "# ============================================================",
    "",

    paste0(
      "FLASH_OUTPUT_DIR = ",
      py_string(output_dir)
    ),

    paste0(
      "FLASH_FILE_NAME = ",
      py_string(file_name)
    ),

    paste0(
      "FLASH_COLOR_MODE = ",
      py_string(color_mode)
    ),

    paste0(
      "FLASH_SLICE_VIEW = ",
      py_string(slice_view)
    ),
    "",

    "FLASH_SECTION_POINTS = {",
    section_lines,
    "}",
    "",
    "FLASH_CAPTURE_SECTIONS = [",
    capture_lines,
    "]",
    "",
    paste0(
      "FLASH_REFERENCE_TOLERANCE_MM = ",
      fmt_num(reference_tolerance_mm)
    ),
    "",


    # ==========================================================
    # COPY MATRIX
    # ==========================================================

    "def _fc_copy_matrix(matrix):",
    "    out = vtk.vtkMatrix4x4()",
    "    out.DeepCopy(matrix)",
    "    return out",
    "",


    # ==========================================================
    # GET RED SLICE
    # ==========================================================

    "def _fc_get_slice_view():",
    "    lm = slicer.app.layoutManager()",
    "    widget = lm.sliceWidget(FLASH_SLICE_VIEW)",
    "    if widget is None:",
    "        raise ValueError(",
    "            f'Could not find {FLASH_SLICE_VIEW} slice view.'",
    "        )",
    "    logic = widget.sliceLogic()",
    "    node = logic.GetSliceNode()",
    "    view = widget.sliceView()",
    "    return widget, logic, node, view",
    "",


    # ==========================================================
    # CURRENT PLANE GEOMETRY
    # ==========================================================

    "def _fc_plane_geometry(matrix):",
    "",
    "    origin = np.array([",
    "        matrix.GetElement(0, 3),",
    "        matrix.GetElement(1, 3),",
    "        matrix.GetElement(2, 3)",
    "    ], dtype=float)",
    "",
    "    normal = np.array([",
    "        matrix.GetElement(0, 2),",
    "        matrix.GetElement(1, 2),",
    "        matrix.GetElement(2, 2)",
    "    ], dtype=float)",
    "",
    "    n = np.linalg.norm(normal)",
    "",
    "    if n < 1e-12:",
    "        raise ValueError('Current Red slice has a zero normal.')",
    "",
    "    normal /= n",
    "",
    "    return origin, normal",
    "",


    # ==========================================================
    # DETECT WHICH ORIENTCSG SECTION IS CURRENTLY DISPLAYED
    #
    # Pan does not matter here because pan is in-plane.
    # We compare only perpendicular distance to each section.
    # ==========================================================

    "def _fc_detect_reference_section(origin, normal):",
    "",
    "    distances = {}",
    "",
    "    for name, point in FLASH_SECTION_POINTS.items():",
    "        distances[name] = abs(",
    "            float(np.dot(point - origin, normal))",
    "        )",
    "",
    "    reference = min(distances, key=distances.get)",
    "    distance = distances[reference]",
    "",
    "    print(",
    "        f'Flash Capture reference section: {reference} '",
    "        f'(plane distance = {distance:.6f} mm)'",
    "    )",
    "",
    "    if distance > FLASH_REFERENCE_TOLERANCE_MM:",
    "        raise ValueError(",
    "            'The current Red slice is not sufficiently close to one '",
    "            'of the OrientCSG sections. First paste an OrientCSG '",
    "            'section block (for example SECTION_50), then configure '",
    "            'zoom/pan/window-level, and run Flash Capture.'",
    "        )",
    "",
    "    return reference",
    "",


    # ==========================================================
    # SAVE CURRENT ORIENTCSG SCALE POSITION
    # ==========================================================

    "def _fc_get_scale_nodes():",
    "",
    "    nodes = []",
    "",
    "    for className in [",
    "        'vtkMRMLMarkupsLineNode',",
    "        'vtkMRMLMarkupsFiducialNode'",
    "    ]:",
    "",
    "        for node in slicer.util.getNodesByClass(className):",
    "",
    "            name = node.GetName() or ''",
    "",
    "            if name.startswith('OrientCSG_scale'):",
    "                nodes.append(node)",
    "",
    "    return nodes",
    "",


    "def _fc_save_markup_positions(nodes):",
    "",
    "    state = []",
    "",
    "    for node in nodes:",
    "",
    "        points = []",
    "",
    "        for i in range(node.GetNumberOfControlPoints()):",
    "",
    "            p = [0.0, 0.0, 0.0]",
    "            node.GetNthControlPointPositionWorld(i, p)",
    "",
    "            points.append(",
    "                np.array(p, dtype=float)",
    "            )",
    "",
    "        state.append((node, points))",
    "",
    "    return state",
    "",


    "def _fc_apply_markup_delta(state, delta):",
    "",
    "    delta = np.asarray(delta, dtype=float)",
    "",
    "    for node, points in state:",
    "",
    "        for i, p in enumerate(points):",
    "",
    "            q = p + delta",
    "",
    "            node.SetNthControlPointPositionWorld(",
    "                i,",
    "                float(q[0]),",
    "                float(q[1]),",
    "                float(q[2])",
    "            )",
    "",
    "        node.Modified()",
    "",


    # ==========================================================
    # MOVE RED SLICE WHILE PRESERVING CURRENT VISUAL STATE
    #
    # Only the translation component is changed.
    #
    # Therefore the current:",
    #   - orientation",
    "#   - zoom",
    "#   - field of view",
    "#   - in-plane pan",
    "# remains unchanged.",
    # ==========================================================

    "def _fc_apply_section(",
    "    sliceNode,",
    "    baselineMatrix,",
    "    delta",
    "):",
    "",
    "    matrix = _fc_copy_matrix(baselineMatrix)",
    "",
    "    for i in range(3):",
    "",
    "        matrix.SetElement(",
    "            i,",
    "            3,",
    "            baselineMatrix.GetElement(i, 3)",
    "            + float(delta[i])",
    "        )",
    "",
    "    sliceNode.GetSliceToRAS().DeepCopy(matrix)",
    "    sliceNode.UpdateMatrices()",
    "    sliceNode.Modified()",
    "",


    # ==========================================================
    # TIFF SCREENSHOT
    # ==========================================================

    "def _fc_save_tiff(view, filename):",
    "",
    "    slicer.app.processEvents()",
    "    view.forceRender()",
    "    slicer.app.processEvents()",
    "",
    "    w2i = vtk.vtkWindowToImageFilter()",
    "    w2i.SetInput(view.renderWindow())",
    "    w2i.SetInputBufferTypeToRGB()",
    "    w2i.ReadFrontBufferOff()",
    "    w2i.Update()",
    "",
    "    writer = vtk.vtkTIFFWriter()",
    "    writer.SetFileName(filename)",
    "",
    "    if FLASH_COLOR_MODE == 'grayscale':",
    "        luminance = vtk.vtkImageLuminance()",
    "        luminance.SetInputConnection(w2i.GetOutputPort())",
    "        luminance.Update()",
    "        writer.SetInputConnection(luminance.GetOutputPort())",
    "        writer.SetCompressionToDeflate()",
    "    else:",
    "        writer.SetInputConnection(w2i.GetOutputPort())",
    "",
    "    writer.Write()",
    "",


    # ==========================================================
    # MAIN FLASH CAPTURE FUNCTION
    # ==========================================================

    "def flash_capture():",
    "",
    "    os.makedirs(",
    "        FLASH_OUTPUT_DIR,",
    "        exist_ok=True",
    "    )",
    "",
    "    widget, logic, sliceNode, view = _fc_get_slice_view()",
    "",
    "    # --------------------------------------------------------",
    "    # Store the exact Red view prepared by the user",
    "    # --------------------------------------------------------",
    "",
    "    baselineMatrix = _fc_copy_matrix(",
    "        sliceNode.GetSliceToRAS()",
    "    )",
    "",
    "    baselineFOV = tuple(",
    "        sliceNode.GetFieldOfView()",
    "    )",
    "",
    "    baselineOrigin, baselineNormal = _fc_plane_geometry(",
    "        baselineMatrix",
    "    )",
    "",
    "    referenceName = _fc_detect_reference_section(",
    "        baselineOrigin,",
    "        baselineNormal",
    "    )",
    "",
    "    referencePoint = FLASH_SECTION_POINTS[referenceName]",
    "",
    "    scaleNodes = _fc_get_scale_nodes()",
    "    scaleState = _fc_save_markup_positions(scaleNodes)",
    "",
    "    print(",
    "        f'OrientCSG scale nodes found: {len(scaleNodes)}'",
    "    )",
    "",
    "    print(",
    "        'Flash Capture output directory:',",
    "        FLASH_OUTPUT_DIR",
    "    )",
    "",
    "    print(",
    "        'Current Red field of view:',",
    "        baselineFOV",
    "    )",
    "",

    "    try:",
    "",
    "        for sectionName in FLASH_CAPTURE_SECTIONS:",
    "",
    "            targetPoint = FLASH_SECTION_POINTS[sectionName]",
    "",
    "            # Translation from the reference section to",
    "            # the requested section.",
    "            delta = targetPoint - referencePoint",
    "",
    "            _fc_apply_section(",
    "                sliceNode,",
    "                baselineMatrix,",
    "                delta",
    "            )",
    "",
    "            # Move the existing OrientCSG scale by the same",
    "            # world-space translation.",
    "            _fc_apply_markup_delta(",
    "                scaleState,",
    "                delta",
    "            )",
    "",
    "            slicer.app.processEvents()",
    "            view.forceRender()",
    "",
    "            pct = sectionName.replace('SECTION_', '')",
    "",
    "            filename = os.path.join(",
    "                FLASH_OUTPUT_DIR,",
    "                f'{FLASH_FILE_NAME}_{pct}.tif'",
    "            )",
    "",
    "            _fc_save_tiff(",
    "                view,",
    "                filename",
    "            )",
    "",
    "            print('Saved:', filename)",
    "",

    "    finally:",
    "",
    "        # ----------------------------------------------------",
    "        # Restore exactly the view that existed before batch",
    "        # capture.",
    "        # ----------------------------------------------------",
    "",
    "        sliceNode.GetSliceToRAS().DeepCopy(",
    "            baselineMatrix",
    "        )",
    "",
    "        sliceNode.UpdateMatrices()",
    "        sliceNode.Modified()",
    "",
    "        try:",
    "            sliceNode.SetFieldOfView(",
    "                float(baselineFOV[0]),",
    "                float(baselineFOV[1]),",
    "                float(baselineFOV[2])",
    "            )",
    "        except Exception:",
    "            pass",
    "",
    "        _fc_apply_markup_delta(",
    "            scaleState,",
    "            np.zeros(3)",
    "        )",
    "",
    "        slicer.app.processEvents()",
    "        view.forceRender()",
    "",
    "    print(",
    "        f'Flash Capture finished: '",
    "        f'{len(FLASH_CAPTURE_SECTIONS)} sections saved.'",
    "    )",
    "",


    # ==========================================================
    # EXECUTE IMMEDIATELY WHEN PASTED
    # ==========================================================

    "flash_capture()"
  )


  txt <- paste(
    code,
    collapse = "\n"
  )


  # ----------------------------------------------------------
  # COPY PYTHON BLOCK TO WINDOWS CLIPBOARD
  # ----------------------------------------------------------

  if (isTRUE(copy)) {
    copied <- copy_to_clipboard(txt)
    if (isTRUE(copied)) {
      message(sprintf(
        "Slicer CT Flash Capture Python copied to clipboard (%d sections).",
        length(selected)
      ))
    }
  }


  invisible(txt)
}



.flash_capture_slicer_solid <- function(
    res,
    file_name,
    output_dir,
    sections = NULL,
    color_mode = c("rgb", "grayscale"),
    copy = TRUE
) {

  reference_tolerance_mm <- 2
  color_mode <- match.arg(color_mode)

  if (!inherits(res, "orientcsg_longbone")) {
    stop("`res` must be an OrientCSG long-bone result.", call. = FALSE)
  }

  if (!isTRUE(res$SLICER) || !isTRUE(res$SOLID)) {
    stop(
      "Slicer SOLID Flash Capture requires `SLICER = TRUE` and `SOLID = TRUE`.",
      call. = FALSE
    )
  }

  if (is.null(res$section_points) ||
      length(res$section_points) == 0L ||
      is.null(names(res$section_points))) {
    stop("`res` does not contain valid named section points.", call. = FALSE)
  }

  if (is.null(res$vectors$L) || length(res$vectors$L) != 3L) {
    stop("`res` does not contain a valid longitudinal vector.", call. = FALSE)
  }

  if (length(file_name) != 1L || is.na(file_name) || !nzchar(file_name)) {
    stop("`file_name` must be a non-empty character string.", call. = FALSE)
  }

  if (length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` must be a non-empty character string.", call. = FALSE)
  }

  model_name <- res$model_name
  if (is.null(model_name) || length(model_name) != 1L ||
      is.na(model_name) || !nzchar(model_name)) {
    stop("`res$model_name` is required for Slicer SOLID Flash Capture.", call. = FALSE)
  }

  output_dir <- chartr("\\", "/", output_dir)

  available <- names(res$section_points)

  if (is.null(sections)) {
    selected <- available
  } else {
    if (is.numeric(sections)) {
      selected <- paste0("SECTION_", sections)
    } else {
      sections <- as.character(sections)
      selected <- ifelse(
        grepl("^SECTION_", sections),
        sections,
        paste0("SECTION_", sections)
      )
    }

    missing <- setdiff(selected, available)
    if (length(missing) > 0L) {
      stop(
        sprintf(
          "Unknown section(s): %s. Available sections: %s",
          paste(missing, collapse = ", "),
          paste(available, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  selected <- unique(selected)

  flip_xy <- function(v) {
    v <- as.numeric(v)
    c(-v[1], -v[2], v[3])
  }

  points_ras <- lapply(res$section_points, flip_xy)
  L_ras <- flip_xy(res$vectors$L)

  fmt_num <- function(x) {
    formatC(
      as.numeric(x),
      format = "f",
      digits = 9,
      drop0trailing = FALSE
    )
  }

  py_vec <- function(v) {
    paste0(
      "np.array([",
      paste(fmt_num(v), collapse = ", "),
      "], dtype=float)"
    )
  }

  py_string <- function(x) {
    encodeString(as.character(x), quote = "'")
  }

  section_lines <- vapply(
    names(points_ras),
    function(sec) {
      paste0(
        "    ", py_string(sec), ": ", py_vec(points_ras[[sec]]), ","
      )
    },
    character(1)
  )

  capture_lines <- paste0(
    "    ",
    vapply(selected, py_string, character(1)),
    ","
  )

  code <- c(
    "import os",
    "import slicer",
    "import vtk",
    "import numpy as np",
    "",
    "# ============================================================",
    "# OrientCSG FLASH CAPTURE",
    "# 3D Slicer / SOLID mesh",
    "# ============================================================",
    "#",
    "# The CURRENT 3D view is the visual reference.",
    "# The mesh is re-cut at every requested OrientCSG section.",
    "# Camera orientation, proximal viewing side, parallel scale,",
    "# and relative in-plane pan are preserved.",
    "# The starting section and camera are restored afterwards.",
    "# ============================================================",
    "",
    paste0("FLASH_MODEL_NAME = ", py_string(model_name)),
    paste0("FLASH_OUTPUT_DIR = ", py_string(output_dir)),
    paste0("FLASH_FILE_NAME = ", py_string(file_name)),
    paste0("FLASH_COLOR_MODE = ", py_string(color_mode)),
    paste0("FLASH_NORMAL = ", py_vec(L_ras)),
    paste0("FLASH_REFERENCE_TOLERANCE_MM = ", fmt_num(reference_tolerance_mm)),
    "",
    "FLASH_ALL_SECTION_POINTS = {",
    section_lines,
    "}",
    "",
    "FLASH_CAPTURE_SECTIONS = [",
    capture_lines,
    "]",
    "",
    "def _fc_nrm(v):",
    "    v = np.asarray(v, dtype=float)",
    "    s = np.linalg.norm(v)",
    "    if s < 1e-12:",
    "        raise ValueError('Near-zero vector cannot be normalized.')",
    "    return v / s",
    "",
    "def _fc_get_3d_view():",
    "    lm = slicer.app.layoutManager()",
    "    widget = lm.threeDWidget(0)",
    "    if widget is None:",
    "        raise ValueError('Could not find the first 3D view.')",
    "    view = widget.threeDView()",
    "    cameraNode = view.cameraNode()",
    "    if cameraNode is None:",
    "        raise ValueError('Could not find the 3D camera node.')",
    "    return view, cameraNode, cameraNode.GetCamera()",
    "",
    "def _fc_is_output_model(name):",
    "    return (name.startswith('SECTION_') or",
    "            name.startswith('OrientCSG') or",
    "            name.startswith('Estimated_longitudinal_axis'))",
    "",
    "def _fc_get_target_model():",
    "    plausible = []",
    "    for node in slicer.util.getNodesByClass('vtkMRMLModelNode'):",
    "        name = node.GetName() or ''",
    "        pd = node.GetPolyData()",
    "        if pd is None or pd.GetNumberOfPoints() == 0 or pd.GetNumberOfCells() == 0:",
    "            continue",
    "        if _fc_is_output_model(name):",
    "            continue",
    "        plausible.append(node)",
    "",
    "    exact = [node for node in plausible if node.GetName() == FLASH_MODEL_NAME]",
    "    if len(exact) == 1:",
    "        return exact[0]",
    "",
    "    partial = [",
    "        node for node in plausible",
    "        if FLASH_MODEL_NAME.lower() in (node.GetName() or '').lower()",
    "    ]",
    "    if len(partial) == 1:",
    "        print(f'Using partial model-name match: {partial[0].GetName()}')",
    "        return partial[0]",
    "",
    "    if len(plausible) == 1:",
    "        print(f'Auto-selected only plausible model: {plausible[0].GetName()}')",
    "        return plausible[0]",
    "",
    "    print('Available plausible model nodes:')",
    "    for node in plausible:",
    "        print('  ', node.GetName())",
    "    raise ValueError('Could not uniquely select the source mesh model.')",
    "",
    "def _fc_get_model_polydata_world(modelNode):",
    "    polyData = modelNode.GetPolyData()",
    "    transformToWorld = vtk.vtkGeneralTransform()",
    "    slicer.vtkMRMLTransformNode.GetTransformBetweenNodes(",
    "        modelNode.GetParentTransformNode(), None, transformToWorld",
    "    )",
    "    tf = vtk.vtkTransformPolyDataFilter()",
    "    tf.SetInputData(polyData)",
    "    tf.SetTransform(transformToWorld)",
    "    tf.Update()",
    "    out = vtk.vtkPolyData()",
    "    out.DeepCopy(tf.GetOutput())",
    "    return out",
    "",
    "def _fc_cut_section(polyData, point, normal):",
    "    plane = vtk.vtkPlane()",
    "    plane.SetOrigin(float(point[0]), float(point[1]), float(point[2]))",
    "    plane.SetNormal(float(normal[0]), float(normal[1]), float(normal[2]))",
    "",
    "    cutter = vtk.vtkCutter()",
    "    cutter.SetCutFunction(plane)",
    "    cutter.SetInputData(polyData)",
    "    cutter.Update()",
    "",
    "    clean = vtk.vtkCleanPolyData()",
    "    clean.SetInputData(cutter.GetOutput())",
    "    clean.Update()",
    "",
    "    stripper = vtk.vtkStripper()",
    "    stripper.SetInputData(clean.GetOutput())",
    "    stripper.Update()",
    "",
    "    outline = vtk.vtkPolyData()",
    "    outline.DeepCopy(stripper.GetOutput())",
    "",
    "    if outline.GetNumberOfPoints() == 0:",
    "        raise ValueError('The requested section plane did not intersect the mesh.')",
    "",
    "    triangulator = vtk.vtkContourTriangulator()",
    "    triangulator.SetInputData(outline)",
    "    triangulator.Update()",
    "",
    "    filled = vtk.vtkPolyData()",
    "    filled.DeepCopy(triangulator.GetOutput())",
    "",
    "    if filled.GetNumberOfCells() == 0:",
    "        raise ValueError('Could not triangulate the requested mesh section.')",
    "",
    "    return filled",
    "",
    "def _fc_deepcopy_polydata(polyData):",
    "    out = vtk.vtkPolyData()",
    "    out.DeepCopy(polyData)",
    "    return out",
    "",
    "def _fc_detect_reference_section(focalPoint, normal):",
    "    distances = {}",
    "    for name, point in FLASH_ALL_SECTION_POINTS.items():",
    "        distances[name] = abs(float(np.dot(point - focalPoint, normal)))",
    "",
    "    reference = min(distances, key=distances.get)",
    "    distance = distances[reference]",
    "",
    "    print(",
    "        f'Flash Capture reference section: {reference} '",
    "        f'(axial distance = {distance:.6f} mm)'",
    "    )",
    "",
    "    if distance > FLASH_REFERENCE_TOLERANCE_MM:",
    "        raise ValueError(",
    "            'The current 3D camera is not sufficiently close to one '",
    "            'of the OrientCSG section planes. First paste a normal '",
    "            'OrientCSG Slicer SOLID block, configure the 3D view, '",
    "            'and then run Flash Capture.'",
    "        )",
    "",
    "    return reference",
    "",
    "def _fc_get_reference_section_node(referenceName):",
    "    pct = referenceName.replace('SECTION_', '')",
    "    wanted = f'SECTION_{pct}_filled'",
    "    matches = [",
    "        node for node in slicer.util.getNodesByClass('vtkMRMLModelNode')",
    "        if (node.GetName() or '') == wanted",
    "    ]",
    "    if len(matches) != 1:",
    "        raise ValueError(",
    "            f'Expected exactly one model node named {wanted}; found {len(matches)}.'",
    "        )",
    "    return matches[0]",
    "",
    "def _fc_get_movable_axis_nodes(referenceName):",
    "    names = {referenceName + '_ML', referenceName + '_AP'}",
    "    nodes = []",
    "    for node in slicer.util.getNodesByClass('vtkMRMLMarkupsLineNode'):",
    "        if (node.GetName() or '') in names:",
    "            nodes.append(node)",
    "    return nodes",
    "",
    "def _fc_save_markup_positions(nodes):",
    "    state = []",
    "    for node in nodes:",
    "        points = []",
    "        for i in range(node.GetNumberOfControlPoints()):",
    "            p = [0.0, 0.0, 0.0]",
    "            node.GetNthControlPointPositionWorld(i, p)",
    "            points.append(np.array(p, dtype=float))",
    "        state.append((node, points))",
    "    return state",
    "",
    "def _fc_apply_markup_delta(state, delta):",
    "    delta = np.asarray(delta, dtype=float)",
    "    for node, points in state:",
    "        for i, p in enumerate(points):",
    "            q = p + delta",
    "            node.SetNthControlPointPositionWorld(",
    "                i, float(q[0]), float(q[1]), float(q[2])",
    "            )",
    "        node.Modified()",
    "",
    "def _fc_set_camera(cameraNode, camera, position, focal, viewUp, parallelScale):",
    "    camera.SetPosition(float(position[0]), float(position[1]), float(position[2]))",
    "    camera.SetFocalPoint(float(focal[0]), float(focal[1]), float(focal[2]))",
    "    camera.SetViewUp(float(viewUp[0]), float(viewUp[1]), float(viewUp[2]))",
    "    camera.SetParallelScale(float(parallelScale))",
    "    camera.ParallelProjectionOn()",
    "    camera.OrthogonalizeViewUp()",
    "    cameraNode.Modified()",
    "",
    "def _fc_save_tiff(view, filename):",
    "    slicer.app.processEvents()",
    "    view.forceRender()",
    "    slicer.app.processEvents()",
    "",
    "    w2i = vtk.vtkWindowToImageFilter()",
    "    w2i.SetInput(view.renderWindow())",
    "    w2i.SetInputBufferTypeToRGB()",
    "    w2i.ReadFrontBufferOff()",
    "    w2i.Update()",
    "",
    "    writer = vtk.vtkTIFFWriter()",
    "    writer.SetFileName(filename)",
    "",
    "    if FLASH_COLOR_MODE == 'grayscale':",
    "        luminance = vtk.vtkImageLuminance()",
    "        luminance.SetInputConnection(w2i.GetOutputPort())",
    "        luminance.Update()",
    "        writer.SetInputConnection(luminance.GetOutputPort())",
    "        writer.SetCompressionToDeflate()",
    "    else:",
    "        writer.SetInputConnection(w2i.GetOutputPort())",
    "",
    "    writer.Write()",
    "",
    "def flash_capture():",
    "    os.makedirs(FLASH_OUTPUT_DIR, exist_ok=True)",
    "",
    "    normal = _fc_nrm(FLASH_NORMAL)",
    "    view, cameraNode, camera = _fc_get_3d_view()",
    "",
    "    baselinePosition = np.array(camera.GetPosition(), dtype=float)",
    "    baselineFocal = np.array(camera.GetFocalPoint(), dtype=float)",
    "    baselineViewUp = np.array(camera.GetViewUp(), dtype=float)",
    "    baselineParallelScale = float(camera.GetParallelScale())",
    "    baselineParallelProjection = bool(camera.GetParallelProjection())",
    "    baselineClipping = tuple(camera.GetClippingRange())",
    "",
    "    referenceName = _fc_detect_reference_section(baselineFocal, normal)",
    "    referencePoint = FLASH_ALL_SECTION_POINTS[referenceName]",
    "",
    "    sourceModel = _fc_get_target_model()",
    "    sourcePolyDataWorld = _fc_get_model_polydata_world(sourceModel)",
    "",
    "    sectionNode = _fc_get_reference_section_node(referenceName)",
    "    baselineSectionPolyData = _fc_deepcopy_polydata(sectionNode.GetPolyData())",
    "",
    "    movableAxes = _fc_get_movable_axis_nodes(referenceName)",
    "    axisState = _fc_save_markup_positions(movableAxes)",
    "",
    "    print('Flash Capture source model:', sourceModel.GetName())",
    "    print('Flash Capture output directory:', FLASH_OUTPUT_DIR)",
    "    print('Reference section:', referenceName)",
    "    print('Sections to capture:', FLASH_CAPTURE_SECTIONS)",
    "    print('Movable ML/AP axis nodes found:', len(movableAxes))",
    "",
    "    try:",
    "        for sectionName in FLASH_CAPTURE_SECTIONS:",
    "            targetPoint = FLASH_ALL_SECTION_POINTS[sectionName]",
    "            delta = targetPoint - referencePoint",
    "",
    "            filled = _fc_cut_section(",
    "                sourcePolyDataWorld, targetPoint, normal",
    "            )",
    "            sectionNode.SetAndObservePolyData(filled)",
    "            sectionNode.Modified()",
    "",
    "            _fc_apply_markup_delta(axisState, delta)",
    "",
    "            _fc_set_camera(",
    "                cameraNode,",
    "                camera,",
    "                baselinePosition + delta,",
    "                baselineFocal + delta,",
    "                baselineViewUp,",
    "                baselineParallelScale",
    "            )",
    "",
    "            camera.SetClippingRange(*baselineClipping)",
    "            cameraNode.Modified()",
    "            slicer.app.processEvents()",
    "            view.forceRender()",
    "",
    "            pct = sectionName.replace('SECTION_', '')",
    "            filename = os.path.join(",
    "                FLASH_OUTPUT_DIR,",
    "                f'{FLASH_FILE_NAME}_{pct}.tif'",
    "            )",
    "",
    "            _fc_save_tiff(view, filename)",
    "            print('Saved:', filename)",
    "",
    "    finally:",
    "        sectionNode.SetAndObservePolyData(baselineSectionPolyData)",
    "        sectionNode.Modified()",
    "",
    "        _fc_apply_markup_delta(axisState, np.zeros(3))",
    "",
    "        camera.SetPosition(*baselinePosition)",
    "        camera.SetFocalPoint(*baselineFocal)",
    "        camera.SetViewUp(*baselineViewUp)",
    "        camera.SetParallelScale(baselineParallelScale)",
    "        if baselineParallelProjection:",
    "            camera.ParallelProjectionOn()",
    "        else:",
    "            camera.ParallelProjectionOff()",
    "        camera.SetClippingRange(*baselineClipping)",
    "        camera.OrthogonalizeViewUp()",
    "        cameraNode.Modified()",
    "",
    "        slicer.app.processEvents()",
    "        view.forceRender()",
    "",
    "    print(",
    "        f'Flash Capture finished: '",
    "        f'{len(FLASH_CAPTURE_SECTIONS)} sections saved.'",
    "    )",
    "",
    "flash_capture()"
  )

  txt <- paste(code, collapse = "\n")

  if (isTRUE(copy)) {
    copied <- copy_to_clipboard(txt)
    if (isTRUE(copied)) {
      message(sprintf(
        "Slicer SOLID Flash Capture Python copied to clipboard (%d sections).",
        length(selected)
      ))
    }
  }

  invisible(txt)
}
