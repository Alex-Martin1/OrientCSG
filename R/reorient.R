#' Generate anatomical reorientation code for a long bone
#'
#' `Reorient()` generates a software-specific command block that places a
#' long-bone result in a Cartesian anatomical reference system and copies the
#' block directly to the system clipboard. The longitudinal axis is aligned with
#' Z from distal to proximal, the anterior direction with Y, and the distal
#' biomechanical origin with `(0, 0, 0)`.
#'
#' The function uses the geometry already stored in an [orient_longbone()]
#' result and does not recompute landmarks, biomechanical length, or anatomical
#' axes. Full anatomical orientation (`USE_ANAT_ORIENT = TRUE`) is required.
#'
#' @section Surface meshes:
#' For 3D Slicer mesh-input workflows, `Reorient()` creates a new model with the
#' suffix `_Anatomical`. The transformed vertex coordinates are written into the
#' new model itself, while the source model is left unchanged. The new model can
#' therefore be saved or exported as an anatomically reoriented mesh.
#'
#' @section CT volumes:
#' CT support is intentionally more limited. In 3D Slicer, `Reorient()` places
#' the volume under a linear anatomical transform and aligns the standard slice
#' viewers with the transformed axes, but it does not resample the underlying
#' voxel lattice. In Avizo/Amira, it generates only the spatial `setTransform`
#' step. These CT operations should therefore be treated as non-resampled,
#' provisional spatial/display reorientations rather than as newly reoriented
#' DICOM volumes.
#'
#' Creating a CT volume whose voxel grid and DICOM slices are intrinsically
#' aligned with the anatomical axes requires resampling in the external imaging
#' software. Neither `Reorient()` nor OrientCSG performs that resampling. In
#' Avizo/Amira, this can be done manually with `Resample Transformed Image` if
#' required.
#'
#' @param res An `orientcsg_longbone` result returned by [orient_longbone()].
#'   Full anatomical orientation must have been used.
#' @param object_name Optional name of the target model or volume in the external
#'   application. If omitted, `Reorient()` uses `res$model_name` for mesh-input Slicer
#'   workflows, `res$volume_name` for volume-input workflows, and then
#'   `res$individual_id` as a fallback.
#'
#' @return Invisibly returns the generated TCL or Python command block as a
#'   character scalar after copying it to the system clipboard.
#'
#' @examples
#' \dontrun{
#' Reorient(res)
#' Reorient(res, object_name = "W30")
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

  object_name <- .reorient_object_name(res, object_name)
  origin <- .reorient_distal_origin(res)
  signs <- slicer_longbone_screen_signs(res$type, TRUE)

  if (isTRUE(res$SLICER)) {
    if (identical(res$INPUT, "MESH")) {
      return(.reorient_slicer_model_python(res, origin, signs, object_name))
    }
    return(.reorient_slicer_volume_python(res, origin, signs, object_name))
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

.reorient_object_name <- function(res, object_name = NULL) {
  if (!is.null(object_name)) {
    if (length(object_name) != 1L || is.na(object_name) || !nzchar(trimws(object_name))) {
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
    stop("Could not construct an anatomical Y axis orthogonal to X.", call. = FALSE)
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

.reorient_slicer_volume_python <- function(res, origin, signs, object_name) {
  code <- c(
    "import slicer",
    "import vtk",
    "import qt",
    "import numpy as np",
    "",
    "# ============================================================",
    "# OrientCSG: anatomical reorientation of a CT volume",
    "# No resampling: voxel values and HU are unchanged.",
    "# ============================================================",
    paste0("VOLUME_NAME = ", .reorient_py_quote(object_name)),
    paste0("LONG_LPS = ", .reorient_py_vector(res$vectors$L)),
    paste0("ML_LPS = ", .reorient_py_vector(res$vectors$ML)),
    paste0("AP_LPS = ", .reorient_py_vector(res$vectors$AP)),
    paste0("ORIGIN_LPS = ", .reorient_py_vector(origin)),
    paste0("BIO_LENGTH = ", .reorient_fmt(res$biomechanical_length)),
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
    "redWidget.sliceLogic().SetSliceOffset(0.50 * BIO_LENGTH)",
    "yellowWidget.sliceLogic().SetSliceOffset(0.0)",
    "greenWidget.sliceLogic().SetSliceOffset(0.0)",
    "",
    "print('\\n============================================')",
    "print('ANATOMICAL TRANSFORM APPLIED')",
    "print('============================================')",
    "print('Volume:', volumeNode.GetName())",
    "print('X = mediolateral anatomical display axis')",
    "print('Y = anterior')",
    "print('Z = longitudinal, distal -> proximal')",
    "print('Distal biomechanical origin = (0, 0, 0)')",
    "print('Red = transverse section at 50% biomechanical length')",
    "print('Yellow = longitudinal AP plane')",
    "print('Green = longitudinal ML plane')",
    "print('No voxel resampling was performed.')",
    "print('============================================\\n')"
  )

  paste(code, collapse = "\n")
}

.reorient_slicer_model_python <- function(res, origin, signs, object_name) {
  code <- c(
    "import slicer",
    "import vtk",
    "import qt",
    "import numpy as np",
    "",
    "# ============================================================",
    "# OrientCSG: anatomical reorientation of a surface model",
    "# The source model is left unchanged.",
    "# ============================================================",
    paste0("MODEL_NAME = ", .reorient_py_quote(object_name)),
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
    "print('X = mediolateral anatomical display axis')",
    "print('Y = anterior')",
    "print('Z = longitudinal, distal -> proximal')",
    "print('Distal biomechanical origin = (0, 0, 0)')",
    "print('Source model was not modified.')",
    "print('============================================\\n')"
  )

  paste(code, collapse = "\n")
}

.reorient_avizo_tcl <- function(res, origin, signs, object_name) {
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
    "echo \"X = mediolateral anatomical display axis\"",
    "echo \"Y = anterior\"",
    "echo \"Z = longitudinal, distal -> proximal\"",
    "echo \"Distal biomechanical origin = (0, 0, 0)\"",
    "echo \"Volume has NOT been resampled.\"",
    "echo \"For an axis-aligned voxel lattice, use Geometry Transforms > Resample Transformed Image manually.\"",
    "echo \"Recommended Mode: extended.\"",
    "echo \"============================================\""
  )

  paste(code, collapse = "\n")
}
