#' Generate anatomical reorientation code for long bones and mandibles
#'
#' `Reorient()` generates software-specific code that places a long bone or
#' mandible in a standardized Cartesian frame and attempts to copy it to the clipboard.
#' For long bones, Z is distal-to-proximal and the distal biomechanical
#' origin becomes `(0, 0, 0)`.
#' For `HUMERUS` and `ULNA`, Y is anatomically anterior because AP sign is
#' resolved from landmarks. For `TIBIA`, `FEMUR`, and `RADIUS`, Y follows the
#' AP-oriented axis stored by [orient_longbone()], whose sign depends on the
#' acquisition/coordinate convention rather than an AP-defining landmark.
#'
#' For long bones, the function uses the geometry already stored in an
#' [orient_longbone()] result and does not recompute landmarks, biomechanical
#' length, or anatomical axes. Full anatomical orientation
#' (`USE_ANAT_ORIENT = TRUE`) is required. For mandibles, it uses the anatomical
#' vectors and LM2 already stored in an [orient_mandible()] result.
#'
#' @section Surface meshes:
#' For 3D Slicer mesh-input workflows, `Reorient()` creates a new model with the
#' suffix `_Anatomical`. The transformed vertex coordinates are written into the
#' new model itself, while the source model is left unchanged. The new model can
#' therefore be saved or exported as a mesh in the standardized OrientCSG frame.
#' For mandibular results, the input representation is taken from `res$INPUT`.
#' `INPUT = "MESH"` selects the Slicer mesh route automatically.
#'
#' @section CT volumes:
#' `Reorient()` does not resample CT voxels. In 3D Slicer, it applies a
#' linear anatomical transform and aligns the slice viewers; in
#' Avizo/Amira, it generates the corresponding `setTransform` command.
#'
#' An intrinsically reoriented CT voxel grid or DICOM volume requires
#' resampling in the imaging software (e.g., `Resample Transformed Image`
#' in Avizo/Amira).
#'
#' @section Mandibles:
#' Mandibular X/Y/Z point anatomical left-to-right, posterior-to-anterior,
#' and inferior-to-superior, with LM2 (infradentale) at the origin.
#' `Reorient(res)` uses the `INPUT` and `SLICER` settings saved by
#' [orient_mandible()]. CT is available in Slicer and Avizo/Amira; meshes are
#' supported only in Slicer. The model and landmarks must share physical coordinates.
#' Older mandibular results without `INPUT` are treated as CT volumes.
#'
#' @param res An `orientcsg_longbone` or `orientcsg_mandible` result.
#'   For long bones, full anatomical orientation must have been used.
#' @param object_name Optional exact model/volume name for all workflows.
#'   When supplied, Slicer requires an exact match and does not fall back to
#'   another node. When omitted, the name is recovered from `res`; mandibular
#'   MESH results require an exact `res$model_name`, while CT retains the
#'   existing Slicer selection fallback.
#' @return Invisibly returns the generated TCL or Python code as a character
#'   scalar and attempts to copy it to the system clipboard.
#'
#' @examples
#' \dontrun{
#' Reorient(res)
#' Reorient(res, object_name = "W30")
#' Reorient(mandible_res) # Uses the input representation stored in res
#' }
#'
#' @export
Reorient <- function(res, object_name = NULL) {
  txt <- .reorient_code(res, object_name = object_name)
  copy_to_clipboard(txt)

  if (isTRUE(res$SLICER)) {
    message("3D Slicer reorientation code copied to clipboard.")
  } else {
    message("Avizo/Amira reorientation TCL copied to clipboard.")
  }

  invisible(txt)
}

.reorient_code <- function(res, object_name = NULL) {
  if (inherits(res, "orientcsg_mandible")) {
    return(.reorient_mandible_code(res, object_name))
  }

  if (!inherits(res, "orientcsg_longbone")) {
    stop("`res` must be an OrientCSG long-bone result.", call. = FALSE)
  }

  if (is.null(res$INPUT) || length(res$INPUT) != 1L ||
      !res$INPUT %in% c("VOLUME", "MESH")) {
    stop('`res$INPUT` must be either "VOLUME" or "MESH".', call. = FALSE)
  }

  if (!isTRUE(res$USE_ANAT_ORIENT)) {
    stop("`Reorient()` requires `USE_ANAT_ORIENT = TRUE`.", call. = FALSE)
  }

  if (!res$type %in% c("TIBIA", "HUMERUS", "FEMUR", "RADIUS", "ULNA")) {
    stop(
      '`Reorient()` supports "TIBIA", "HUMERUS", "FEMUR", "RADIUS", and "ULNA".',
      call. = FALSE
    )
  }

  if (!is.null(res$internal_coord_system) && !identical(res$internal_coord_system, "LPS")) {
    stop("`Reorient()` currently requires OrientCSG internal LPS geometry.", call. = FALSE)
  }

  if (is.null(res$vectors$L) || is.null(res$vectors$ML) || is.null(res$vectors$AP)) {
    stop("`res` does not contain complete L/ML/AP anatomical vectors.", call. = FALSE)
  }

  strict_name <- !is.null(object_name)
  object_name <- .reorient_object_name(res, object_name)
  origin <- .reorient_distal_origin(res)
  signs <- slicer_longbone_screen_signs(res$type, TRUE)

  if (isTRUE(res$SLICER)) {
    if (identical(res$INPUT, "MESH")) {
      return(.reorient_slicer_model_python(
        res, origin, signs, object_name, strict_name = strict_name
      ))
    }
    return(.reorient_slicer_volume_python(
      res, origin, signs, object_name, strict_name = strict_name
    ))
  }

  if (identical(res$INPUT, "MESH")) {
    stop("Avizo/Amira mesh-input reorientation is not supported.", call. = FALSE)
  }

  if (!nzchar(object_name)) {
    stop(
      "Avizo/Amira reorientation requires a target object name. Supply `object_name` or set `volume_name`/`individual_id` in the source result.",
      call. = FALSE
    )
  }

  .reorient_avizo_tcl(res, origin, signs, object_name)
}

# Select mandibular CT or mesh reorientation using the source result.
.reorient_mandible_code <- function(res, object_name) {
  if (!identical(res$internal_coord_system, "LPS")) {
    stop("Mandibular `Reorient()` requires internal LPS geometry.", call. = FALSE)
  }
  if (!is.logical(res$SLICER) || length(res$SLICER) != 1L ||
      is.na(res$SLICER)) {
    stop("`res$SLICER` must be TRUE or FALSE.", call. = FALSE)
  }
  input <- if (is.null(res$INPUT)) "VOLUME" else res$INPUT
  if (!is.character(input) || length(input) != 1L || is.na(input) ||
      !input %in% c("VOLUME", "MESH")) {
    stop('`res$INPUT` must be either "VOLUME" or "MESH".', call. = FALSE)
  }
  if (identical(input, "MESH") && !res$SLICER) {
    stop('Mandibular `INPUT = "MESH"` requires 3D Slicer.', call. = FALSE)
  }

  geometry <- .reorient_mandible_geometry(res)
  signs <- list(ml_right_sign = 1, anterior_up_sign = 1)
  strict_name <- !is.null(object_name) || identical(input, "MESH")
  object_name <- .reorient_object_name(res, object_name)

  if (identical(input, "MESH")) {
    return(.reorient_slicer_model_python(
      geometry$axes, geometry$origin, signs, object_name,
      mandible = TRUE, strict_name = strict_name
    ))
  }

  if (res$SLICER) {
    return(.reorient_slicer_volume_python(
      geometry$axes, geometry$origin, signs, object_name,
      mandible = TRUE, strict_name = strict_name
    ))
  }

  if (!nzchar(object_name)) {
    stop("Avizo/Amira reorientation requires `object_name` or a source volume/individual name.", call. = FALSE)
  }
  .reorient_avizo_tcl(
    geometry$axes, geometry$origin, signs, object_name, mandible = TRUE
  )
}

.reorient_mandible_geometry <- function(res) {
  LM <- res$landmarks
  if (!is.matrix(LM) || !is.numeric(LM) ||
      !"LM2" %in% rownames(LM) || ncol(LM) != 3L ||
      any(!is.finite(LM["LM2", ]))) {
    stop("Mandibular result must contain finite LM2 coordinates.", call. = FALSE)
  }

  v <- res$vectors
  required <- c("Vec_LeftToRight", "Anterior_ref", "Vec_Penp")
  if (!is.list(v) || any(!vapply(required, function(nm) {
    x <- v[[nm]]
    is.numeric(x) && length(x) == 3L && all(is.finite(x)) &&
      sqrt(sum(x^2)) > 1e-12
  }, logical(1)))) {
    stop("Mandibular result has incomplete anatomical vectors.", call. = FALSE)
  }

  X <- nrm(as.numeric(v$Vec_LeftToRight))
  Y <- as.numeric(v$Anterior_ref)
  Y <- Y - dot3(X, Y) * X
  if (sqrt(sum(Y^2)) < 1e-12) {
    stop("Mandibular right-left and anterior axes are collinear.", call. = FALSE)
  }
  Y <- nrm(Y)
  Z <- nrm(cross3(X, Y))
  superior <- nrm(as.numeric(v$Vec_Penp))

  # Never flip X to force handedness: that would invert anatomical laterality.
  if (dot3(Z, superior) < 0.99) {
    stop(
      "Mandibular right-left/anterior axes disagree with the superior reference.",
      call. = FALSE
    )
  }

  list(
    origin = as.numeric(LM["LM2", ]),
    axes = list(
      vectors = list(L = Z, ML = X, AP = Y),
      biomechanical_length = 0
    )
  )
}

.reorient_object_name <- function(res, object_name = NULL) {
  if (!is.null(object_name)) {
    if (!is.character(object_name) || length(object_name) != 1L ||
        is.na(object_name) || !nzchar(trimws(object_name))) {
      stop("`object_name` must be a single non-empty character value.", call. = FALSE)
    }
    return(trimws(object_name))
  }

  candidate <- if (identical(res$INPUT, "MESH")) res$model_name else res$volume_name

  if (is.null(candidate) || length(candidate) != 1L || is.na(candidate) || !nzchar(trimws(candidate))) {
    candidate <- res$individual_id
  }

  if (is.null(candidate) || length(candidate) != 1L || is.na(candidate) || !nzchar(trimws(candidate))) {
    return("")
  }

  trimws(candidate)
}

.reorient_distal_origin <- function(res) {
  origin <- switch(
    res$type,
    TIBIA = res$projected$Proj_TibioTalar,
    HUMERUS = res$projected$Proj_LM3,
    FEMUR = res$projected$Proj_CondyleMidpoint,
    RADIUS = res$projected$Proj_DistArticular,
    ULNA = res$projected$Proj_UlnarHeadDistal,
    NULL
  )

  if (is.null(origin) || length(origin) != 3L || any(!is.finite(origin))) {
    stop("Could not recover the distal biomechanical origin from `res$projected`.", call. = FALSE)
  }

  as.numeric(origin)
}

.reorient_basis_lps <- function(res, signs) {
  X <- signs$ml_right_sign * as.numeric(res$vectors$ML)
  Y <- signs$anterior_up_sign * as.numeric(res$vectors$AP)
  Z_reference <- nrm(as.numeric(res$vectors$L))

  X <- nrm(X)
  Y <- Y - dot3(Y, X) * X

  if (sqrt(sum(Y^2)) < 1e-12) {
    stop("Could not construct a Y axis orthogonal to X.", call. = FALSE)
  }

  Y <- nrm(Y)
  Z <- nrm(cross3(X, Y))

  if (dot3(Z, Z_reference) < 0) {
    X <- -X
    Z <- nrm(cross3(X, Y))
  }

  list(X = X, Y = Y, Z = Z)
}

.reorient_fmt <- function(x) {
  sprintf("%.15g", as.numeric(x))
}

.reorient_py_quote <- function(x) {
  encodeString(x, quote = '"')
}

.reorient_py_vector <- function(x) {
  paste0("np.array([", paste(.reorient_fmt(x), collapse = ", "), "], dtype=float)")
}

.reorient_slicer_volume_python <- function(res, origin, signs, object_name,
                                            mandible = FALSE, strict_name = FALSE) {
  code <- c(
    "import slicer",
    "import vtk",
    "import qt",
    "import numpy as np",
    "",
    "# ============================================================",
    if (mandible) "# OrientCSG: anatomical reorientation of a mandibular CT volume" else
      "# OrientCSG: anatomical reorientation of a CT volume",
    "# No resampling: voxel values and HU are unchanged.",
    "# ============================================================",
    paste0("VOLUME_NAME = ", .reorient_py_quote(object_name)),
    paste0("STRICT_NAME = ", if (strict_name) "True" else "False"),
    paste0("LONG_LPS = ", .reorient_py_vector(res$vectors$L)),
    paste0("ML_LPS = ", .reorient_py_vector(res$vectors$ML)),
    paste0("AP_LPS = ", .reorient_py_vector(res$vectors$AP)),
    paste0("ORIGIN_LPS = ", .reorient_py_vector(origin)),
    if (!mandible) paste0("BIO_LENGTH = ", .reorient_fmt(res$biomechanical_length)),
    paste0("ML_SIGN = ", .reorient_fmt(signs$ml_right_sign)),
    paste0("AP_SIGN = ", .reorient_fmt(signs$anterior_up_sign)),
    "",
    "volumes = []",
    "for node in slicer.util.getNodesByClass('vtkMRMLScalarVolumeNode'):",
    "    try:",
    "        if node.IsA('vtkMRMLLabelMapVolumeNode'):",
    "            continue",
    "    except Exception:",
    "        pass",
    "    try:",
    "        if node.GetHideFromEditors():",
    "            continue",
    "    except Exception:",
    "        pass",
    "    volumes.append(node)",
    "",
    "exactMatches = [node for node in volumes if node.GetName() == VOLUME_NAME]",
    "",
    "if len(exactMatches) == 1:",
    "    volumeNode = exactMatches[0]",
    "elif len(exactMatches) > 1:",
    "    raise RuntimeError(\"More than one volume is named '%s'.\" % VOLUME_NAME)",
    "elif STRICT_NAME:",
    "    raise RuntimeError(\"No volume named '%s' was found.\" % VOLUME_NAME)",
    "elif len(volumes) == 1:",
    "    volumeNode = volumes[0]",
    "    print(\"WARNING: '%s' not found. Using '%s'.\" % (VOLUME_NAME, volumeNode.GetName()))",
    "elif len(volumes) > 1:",
    "    names = [node.GetName() for node in volumes]",
    "    selectedName, ok = qt.QInputDialog.getItem(",
    "        slicer.util.mainWindow(),",
    "        'OrientCSG: select volume',",
    "        'CT volume:',",
    "        names,",
    "        0,",
    "        False",
    "    )",
    "    if not ok:",
    "        raise RuntimeError('Operation cancelled.')",
    "    volumeNode = volumes[names.index(selectedName)]",
    "else:",
    "    raise RuntimeError('No scalar volume found.')",
    "",
    "if volumeNode.GetParentTransformNode() is not None:",
    "    raise RuntimeError('The volume already has a parent transform.')",
    "",
    "LPS_TO_RAS = np.diag([-1.0, -1.0, 1.0])",
    "X = LPS_TO_RAS @ (ML_SIGN * ML_LPS)",
    "Y = LPS_TO_RAS @ (AP_SIGN * AP_LPS)",
    "Z_REFERENCE = LPS_TO_RAS @ LONG_LPS",
    "origin = LPS_TO_RAS @ ORIGIN_LPS",
    "",
    "X /= np.linalg.norm(X)",
    "Y = Y - np.dot(Y, X) * X",
    "Y /= np.linalg.norm(Y)",
    "Z = np.cross(X, Y)",
    "Z /= np.linalg.norm(Z)",
    "",
    "if np.dot(Z, Z_REFERENCE) < 0:",
    "    X = -X",
    "    Z = np.cross(X, Y)",
    "    Z /= np.linalg.norm(Z)",
    "",
    "R = np.vstack([X, Y, Z])",
    "t = -R @ origin",
    "",
    "M = np.eye(4)",
    "M[:3, :3] = R",
    "M[:3, 3] = t",
    "",
    "transformNode = slicer.mrmlScene.AddNewNodeByClass(",
    "    'vtkMRMLTransformNode',",
    "    slicer.mrmlScene.GenerateUniqueName(volumeNode.GetName() + '_AnatomicalTransform')",
    ")",
    "transformNode.SetMatrixTransformToParent(slicer.util.vtkMatrixFromArray(M))",
    "volumeNode.SetAndObserveTransformNodeID(transformNode.GetID())",
    "",
    "layoutManager = slicer.app.layoutManager()",
    "redWidget = layoutManager.sliceWidget('Red')",
    "yellowWidget = layoutManager.sliceWidget('Yellow')",
    "greenWidget = layoutManager.sliceWidget('Green')",
    "redNode = redWidget.mrmlSliceNode()",
    "yellowNode = yellowWidget.mrmlSliceNode()",
    "greenNode = greenWidget.mrmlSliceNode()",
    "redNode.SetOrientation('Axial')",
    "yellowNode.SetOrientation('Sagittal')",
    "greenNode.SetOrientation('Coronal')",
    "slicer.util.setSliceViewerLayers(background=volumeNode)",
    "redWidget.sliceLogic().FitSliceToAll()",
    "yellowWidget.sliceLogic().FitSliceToAll()",
    "greenWidget.sliceLogic().FitSliceToAll()",
    if (mandible) "redWidget.sliceLogic().SetSliceOffset(0.0)" else
      "redWidget.sliceLogic().SetSliceOffset(0.50 * BIO_LENGTH)",
    "yellowWidget.sliceLogic().SetSliceOffset(0.0)",
    "greenWidget.sliceLogic().SetSliceOffset(0.0)",
    "",
    "print('\\n============================================')",
    "print('ANATOMICAL TRANSFORM APPLIED')",
    "print('============================================')",
    "print('Volume:', volumeNode.GetName())",
    if (mandible) "print('X = anatomical left -> right')" else
      "print('X = mediolateral-oriented transverse axis')",
    if (mandible) "print('Y = anatomical posterior -> anterior')" else
      "print('Y = AP-oriented axis; anatomically anterior only when landmark-resolved')",
    if (mandible) "print('Z = anatomical inferior -> superior')" else
      "print('Z = longitudinal, distal -> proximal')",
    if (mandible) "print('LM2 (infradentale) = (0, 0, 0)')" else
      "print('Distal biomechanical origin = (0, 0, 0)')",
    if (mandible) "print('Red = axial plane through LM2')" else
      "print('Red = transverse section at 50% biomechanical length')",
    if (mandible) "print('Yellow = sagittal plane through LM2')" else
      "print('Yellow = longitudinal AP plane')",
    if (mandible) "print('Green = coronal plane through LM2')" else
      "print('Green = longitudinal ML plane')",
    "print('No voxel resampling was performed.')",
    "print('============================================\\n')"
  )

  paste(code, collapse = "\n")
}

.reorient_slicer_model_python <- function(res, origin, signs, object_name,
                                           mandible = FALSE, strict_name = FALSE) {
  code <- c(
    "import slicer",
    "import vtk",
    "import qt",
    "import numpy as np",
    "",
    "# ============================================================",
    if (mandible) "# OrientCSG: anatomical reorientation of a mandibular surface model" else
      "# OrientCSG: anatomical reorientation of a surface model",
    "# The source model is left unchanged.",
    "# ============================================================",
    paste0("MODEL_NAME = ", .reorient_py_quote(object_name)),
    paste0("STRICT_NAME = ", if (strict_name) "True" else "False"),
    paste0("LONG_LPS = ", .reorient_py_vector(res$vectors$L)),
    paste0("ML_LPS = ", .reorient_py_vector(res$vectors$ML)),
    paste0("AP_LPS = ", .reorient_py_vector(res$vectors$AP)),
    paste0("ORIGIN_LPS = ", .reorient_py_vector(origin)),
    paste0("ML_SIGN = ", .reorient_fmt(signs$ml_right_sign)),
    paste0("AP_SIGN = ", .reorient_fmt(signs$anterior_up_sign)),
    "",
    "models = []",
    "for node in slicer.util.getNodesByClass('vtkMRMLModelNode'):",
    "    poly = node.GetPolyData()",
    "    if poly is None or poly.GetNumberOfPoints() == 0:",
    "        continue",
    "    try:",
    "        if node.GetHideFromEditors():",
    "            continue",
    "    except Exception:",
    "        pass",
    "    if node.GetName().endswith('_Anatomical'):",
    "        continue",
    "    models.append(node)",
    "",
    "exactMatches = [node for node in models if node.GetName() == MODEL_NAME]",
    "",
    "if len(exactMatches) == 1:",
    "    modelNode = exactMatches[0]",
    "elif len(exactMatches) > 1:",
    "    raise RuntimeError(\"More than one model is named '%s'.\" % MODEL_NAME)",
    "elif STRICT_NAME:",
    "    raise RuntimeError(\"No model named '%s' was found.\" % MODEL_NAME)",
    "elif len(models) == 1:",
    "    modelNode = models[0]",
    "    print(\"WARNING: '%s' not found. Using '%s'.\" % (MODEL_NAME, modelNode.GetName()))",
    "elif len(models) > 1:",
    "    names = [node.GetName() for node in models]",
    "    selectedName, ok = qt.QInputDialog.getItem(",
    "        slicer.util.mainWindow(),",
    "        'OrientCSG: select model',",
    "        'Surface model:',",
    "        names,",
    "        0,",
    "        False",
    "    )",
    "    if not ok:",
    "        raise RuntimeError('Operation cancelled.')",
    "    modelNode = models[names.index(selectedName)]",
    "else:",
    "    raise RuntimeError('No valid model found.')",
    "",
    "polyData = modelNode.GetPolyData()",
    "worldPolyData = vtk.vtkPolyData()",
    "",
    "if modelNode.GetParentTransformNode() is not None:",
    "    transformToWorld = vtk.vtkGeneralTransform()",
    "    slicer.vtkMRMLTransformNode.GetTransformBetweenNodes(",
    "        modelNode.GetParentTransformNode(), None, transformToWorld",
    "    )",
    "    worldFilter = vtk.vtkTransformPolyDataFilter()",
    "    worldFilter.SetInputData(polyData)",
    "    worldFilter.SetTransform(transformToWorld)",
    "    worldFilter.Update()",
    "    worldPolyData.DeepCopy(worldFilter.GetOutput())",
    "else:",
    "    worldPolyData.DeepCopy(polyData)",
    "",
    "LPS_TO_RAS = np.diag([-1.0, -1.0, 1.0])",
    "X = LPS_TO_RAS @ (ML_SIGN * ML_LPS)",
    "Y = LPS_TO_RAS @ (AP_SIGN * AP_LPS)",
    "Z_REFERENCE = LPS_TO_RAS @ LONG_LPS",
    "origin = LPS_TO_RAS @ ORIGIN_LPS",
    "",
    "X /= np.linalg.norm(X)",
    "Y = Y - np.dot(Y, X) * X",
    "Y /= np.linalg.norm(Y)",
    "Z = np.cross(X, Y)",
    "Z /= np.linalg.norm(Z)",
    "",
    "if np.dot(Z, Z_REFERENCE) < 0:",
    "    X = -X",
    "    Z = np.cross(X, Y)",
    "    Z /= np.linalg.norm(Z)",
    "",
    "R = np.vstack([X, Y, Z])",
    "t = -R @ origin",
    "",
    "M = np.eye(4)",
    "M[:3, :3] = R",
    "M[:3, 3] = t",
    "",
    "vtkTransform = vtk.vtkTransform()",
    "vtkTransform.SetMatrix(slicer.util.vtkMatrixFromArray(M))",
    "transformFilter = vtk.vtkTransformPolyDataFilter()",
    "transformFilter.SetInputData(worldPolyData)",
    "transformFilter.SetTransform(vtkTransform)",
    "transformFilter.Update()",
    "",
    "reorientedPolyData = vtk.vtkPolyData()",
    "reorientedPolyData.DeepCopy(transformFilter.GetOutput())",
    "",
    "outputName = slicer.mrmlScene.GenerateUniqueName(modelNode.GetName() + '_Anatomical')",
    "outputNode = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLModelNode', outputName)",
    "outputNode.SetAndObservePolyData(reorientedPolyData)",
    "outputNode.CreateDefaultDisplayNodes()",
    "",
    "print('\\n============================================')",
    "print('ANATOMICAL MODEL CREATED')",
    "print('============================================')",
    "print('Source model:', modelNode.GetName())",
    "print('Output model:', outputNode.GetName())",
    if (mandible) "print('X = anatomical left -> right')" else
      "print('X = mediolateral-oriented transverse axis')",
    if (mandible) "print('Y = anatomical posterior -> anterior')" else
      "print('Y = AP-oriented axis; anatomically anterior only when landmark-resolved')",
    if (mandible) "print('Z = anatomical inferior -> superior')" else
      "print('Z = longitudinal, distal -> proximal')",
    if (mandible) "print('LM2 (infradentale) = (0, 0, 0)')" else
      "print('Distal biomechanical origin = (0, 0, 0)')",
    "print('Source model was not modified.')",
    "print('============================================\\n')"
  )

  paste(code, collapse = "\n")
}

.reorient_avizo_tcl <- function(res, origin, signs, object_name, mandible = FALSE) {
  basis <- .reorient_basis_lps(res, signs)
  R <- rbind(basis$X, basis$Y, basis$Z)
  t <- -as.numeric(R %*% origin)

  M <- diag(4)
  M[1:3, 1:3] <- R
  M[1:3, 4] <- t

  values <- .reorient_fmt(as.vector(M))
  matrix_lines <- vapply(seq(1L, 16L, by = 4L), function(i) {
    paste(values[i:(i + 3L)], collapse = " ")
  }, character(1))

  code <- c(
    "# ============================================================",
    if (mandible) "# OrientCSG: mandibular anatomical transform for Avizo/Amira" else
      "# OrientCSG: anatomical transform for Avizo/Amira",
    "# This applies only the spatial transform. It does not resample.",
    "# ============================================================",
    paste0("set OrientCSG_data ", .reorient_py_quote(object_name)),
    "",
    paste0("$OrientCSG_data setTransform \\\n", paste(matrix_lines, collapse = " \\\n")),
    "",
    "$OrientCSG_data fire",
    "viewer 0 viewAll",
    "",
    "echo \"============================================\"",
    "echo \"ANATOMICAL TRANSFORM APPLIED\"",
    if (mandible) "echo \"X = anatomical left -> right\"" else
      "echo \"X = mediolateral-oriented transverse axis\"",
    if (mandible) "echo \"Y = anatomical posterior -> anterior\"" else
      "echo \"Y = AP-oriented axis; anatomically anterior only when landmark-resolved\"",
    if (mandible) "echo \"Z = anatomical inferior -> superior\"" else
      "echo \"Z = longitudinal, distal -> proximal\"",
    if (mandible) "echo \"LM2 (infradentale) = (0, 0, 0)\"" else
      "echo \"Distal biomechanical origin = (0, 0, 0)\"",
    "echo \"Volume has NOT been resampled.\"",
    "echo \"For an axis-aligned voxel lattice, use Geometry Transforms > Resample Transformed Image manually.\"",
    "echo \"Recommended Mode: extended.\"",
    "echo \"============================================\""
  )

  paste(code, collapse = "\n")
}
