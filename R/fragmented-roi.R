#' Copy a 3D Slicer ROI generator for fragmented solid meshes
#'
#' `get_fragmented_roi()` generates a 3D Slicer Python block for quickly
#' creating an oriented ROI around a selected longitudinal portion of a surface
#' mesh and copies that block directly to the system clipboard.
#'
#' The longitudinal reference used by this helper is `LongMax`, calculated from
#' the two mesh vertices with the greatest Euclidean distance. This is a
#' geometric approximation of longitudinal direction and is not the anatomical
#' or biomechanical length calculated by [orient_longbone()]. The ROI is intended
#' as a convenient starting point for manual cropping in 3D Slicer.
#'
#' The generated Slicer code first looks for a valid model whose name exactly
#' matches `model_name`. If no exact match is found and only one valid model is
#' present, that model is used with a warning in the Slicer Python console. If
#' several valid models are present, Slicer opens an interactive model selector.
#'
#' @param model_name Character scalar giving the expected 3D Slicer model name.
#' @param limits Numeric vector of length two giving the lower and upper
#'   longitudinal ROI limits as percentages of `LongMax`. Defaults to
#'   `c(20, 80)`.
#' @param transverse_margin_mm Non-negative numeric scalar giving the additional
#'   transverse ROI margin, in millimetres, added on each side. Defaults to `2`.
#' @param create_la_line Logical. If `TRUE`, the generated Slicer code also
#'   creates a markup line joining the two `LongMax` endpoints. Defaults to
#'   `FALSE`.
#'
#' @return Invisibly returns `TRUE` if the generated Python block was copied to
#'   the clipboard successfully. If no supported clipboard mechanism is
#'   available, a warning is issued and `FALSE` is returned invisibly.
#'
#' @examples
#' \dontrun{
#' get_fragmented_roi("W30")
#' get_fragmented_roi("W30", limits = c(10, 90), create_la_line = TRUE)
#' }
#'
#' @export
#'
get_fragmented_roi <- function(model_name,
                               limits = c(20, 80),
                               transverse_margin_mm = 2,
                               create_la_line = FALSE) {
  if (!is.character(model_name) || length(model_name) != 1L ||
      is.na(model_name) || !nzchar(model_name)) {
    stop("`model_name` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.numeric(limits) || length(limits) != 2L || any(!is.finite(limits))) {
    stop("`limits` must contain two finite numeric percentages.", call. = FALSE)
  }

  limits <- as.numeric(limits)

  if (limits[1] < 0 || limits[2] > 100 || limits[1] >= limits[2]) {
    stop("`limits` must satisfy 0 <= lower < upper <= 100.", call. = FALSE)
  }

  if (!is.numeric(transverse_margin_mm) || length(transverse_margin_mm) != 1L ||
      !is.finite(transverse_margin_mm) || transverse_margin_mm < 0) {
    stop("`transverse_margin_mm` must be a single non-negative finite number.", call. = FALSE)
  }

  if (!is.logical(create_la_line) || length(create_la_line) != 1L || is.na(create_la_line)) {
    stop("`create_la_line` must be TRUE or FALSE.", call. = FALSE)
  }

  lower <- limits[1]
  upper <- limits[2]

  format_limit_label <- function(x) {
    txt <- format(x, scientific = FALSE, trim = TRUE)
    if (grepl("\\.", txt)) {
      txt <- sub("0+$", "", txt)
      txt <- sub("\\.$", "", txt)
    }
    gsub(".", "p", txt, fixed = TRUE)
  }

  roi_label <- paste0(format_limit_label(lower), "_", format_limit_label(upper))

  py <- paste(
    "import slicer",
    "import vtk",
    "import qt",
    "import numpy as np",
    "from vtk.util.numpy_support import vtk_to_numpy",
    "",
    "# ============================================================",
    "# ORIENTCSG: FRAGMENTED SOLID ROI",
    "# ============================================================",
    paste0("MODEL_NAME = ", py_quote(model_name)),
    paste0("LOWER_PERCENT = ", fmt_num_py(lower)),
    paste0("UPPER_PERCENT = ", fmt_num_py(upper)),
    paste0("TRANSVERSE_MARGIN_MM = ", fmt_num_py(as.numeric(transverse_margin_mm))),
    paste0("CREATE_LA_LINE = ", if (isTRUE(create_la_line)) "True" else "False"),
    paste0("ROI_LABEL = ", py_quote(roi_label)),
    "",
    "# ============================================================",
    "# 1. FIND MODEL",
    "# ============================================================",
    "models = []",
    "for node in slicer.util.getNodesByClass('vtkMRMLModelNode'):",
    "    poly = node.GetPolyData()",
    "    if poly is None or poly.GetNumberOfPoints() < 2:",
    "        continue",
    "    try:",
    "        if node.GetHideFromEditors():",
    "            continue",
    "    except Exception:",
    "        pass",
    "    models.append(node)",
    "",
    "if len(models) == 0:",
    "    raise RuntimeError('No valid 3D model was found in the scene.')",
    "",
    "matches = [m for m in models if m.GetName() == MODEL_NAME]",
    "",
    "if len(matches) > 0:",
    "    modelNode = matches[0]",
    "elif len(models) == 1:",
    "    modelNode = models[0]",
    "    print(\"WARNING: model '%s' was not found. Using the only valid model in the scene: '%s'.\" % (MODEL_NAME, modelNode.GetName()))",
    "else:",
    "    names = [m.GetName() for m in models]",
    "    selectedName, ok = qt.QInputDialog.getItem(",
    "        slicer.util.mainWindow(),",
    "        'OrientCSG: select model',",
    "        \"Model '%s' was not found. Select a 3D model:\" % MODEL_NAME,",
    "        names,",
    "        0,",
    "        False",
    "    )",
    "    if not ok:",
    "        raise RuntimeError('Operation cancelled.')",
    "    modelNode = models[names.index(selectedName)]",
    "",
    "print('\\n============================================')",
    "print('Selected model:', modelNode.GetName())",
    "print('============================================')",
    "",
    "# ============================================================",
    "# 2. GET MESH IN WORLD / RAS COORDINATES",
    "# ============================================================",
    "polyData = modelNode.GetPolyData()",
    "",
    "if modelNode.GetParentTransformNode():",
    "    worldTransform = vtk.vtkGeneralTransform()",
    "    slicer.vtkMRMLTransformNode.GetTransformBetweenNodes(",
    "        modelNode.GetParentTransformNode(),",
    "        None,",
    "        worldTransform",
    "    )",
    "    transformFilter = vtk.vtkTransformPolyDataFilter()",
    "    transformFilter.SetInputData(polyData)",
    "    transformFilter.SetTransform(worldTransform)",
    "    transformFilter.Update()",
    "    worldPolyData = transformFilter.GetOutput()",
    "else:",
    "    worldPolyData = polyData",
    "",
    "points = vtk_to_numpy(worldPolyData.GetPoints().GetData()).astype(np.float64)",
    "print('Number of vertices:', len(points))",
    "",
    "# ============================================================",
    "# 3. CONVEX HULL",
    "# ============================================================",
    "try:",
    "    from scipy.spatial import ConvexHull",
    "    hull = ConvexHull(points)",
    "    hullPoints = points[hull.vertices]",
    "    print('Convex hull vertices:', len(hullPoints))",
    "except Exception as e:",
    "    print('ConvexHull could not be calculated; all vertices will be used.')",
    "    print('Reason:', e)",
    "    hullPoints = points",
    "",
    "from scipy.spatial.distance import cdist",
    "",
    "# ============================================================",
    "# 4. EXACT FARTHEST PAIR / LONGMAX",
    "# ============================================================",
    "def exactFarthestPair(pts, blockSize=512):",
    "    n = len(pts)",
    "    bestDistanceSquared = -1.0",
    "    bestI = None",
    "    bestJ = None",
    "",
    "    for start in range(0, n, blockSize):",
    "        end = min(start + blockSize, n)",
    "        block = pts[start:end]",
    "        d2 = cdist(block, pts, metric='sqeuclidean')",
    "        localIndex = np.argmax(d2)",
    "        iLocal, j = np.unravel_index(localIndex, d2.shape)",
    "        value = d2[iLocal, j]",
    "",
    "        if value > bestDistanceSquared:",
    "            bestDistanceSquared = value",
    "            bestI = start + iLocal",
    "            bestJ = j",
    "",
    "    return pts[bestI], pts[bestJ], np.sqrt(bestDistanceSquared)",
    "",
    "pointA, pointB, longMax = exactFarthestPair(hullPoints)",
    "",
    "# ============================================================",
    "# 5. LONGMAX AXIS",
    "# ============================================================",
    "longMaxAxis = pointB - pointA",
    "longMaxAxis = longMaxAxis / np.linalg.norm(longMaxAxis)",
    "",
    "print('\\nLongMax = %.3f mm' % longMax)",
    "print('LongMax axis = [%.6f, %.6f, %.6f]' % tuple(longMaxAxis))",
    "print('Point A = [%.3f, %.3f, %.3f]' % tuple(pointA))",
    "print('Point B = [%.3f, %.3f, %.3f]' % tuple(pointB))",
    "",
    "# ============================================================",
    "# 6. TRANSVERSE AXES ORTHOGONAL TO LONGMAX",
    "# ============================================================",
    "reference = np.array([0.0, 0.0, 1.0])",
    "if abs(np.dot(reference, longMaxAxis)) > 0.90:",
    "    reference = np.array([0.0, 1.0, 0.0])",
    "",
    "axisX = np.cross(reference, longMaxAxis)",
    "axisX /= np.linalg.norm(axisX)",
    "axisY = np.cross(longMaxAxis, axisX)",
    "axisY /= np.linalg.norm(axisY)",
    "",
    "# ============================================================",
    "# 7. PROJECT MODEL INTO LONGMAX COORDINATE SYSTEM",
    "# ============================================================",
    "relativePoints = points - pointA",
    "coordX = relativePoints @ axisX",
    "coordY = relativePoints @ axisY",
    "coordZ = relativePoints @ longMaxAxis",
    "",
    "zLower = (LOWER_PERCENT / 100.0) * longMax",
    "zUpper = (UPPER_PERCENT / 100.0) * longMax",
    "",
    "inside = (coordZ >= zLower) & (coordZ <= zUpper)",
    "sectionPointsX = coordX[inside]",
    "sectionPointsY = coordY[inside]",
    "",
    "if len(sectionPointsX) == 0:",
    "    raise RuntimeError('No mesh points were found inside the requested longitudinal limits.')",
    "",
    "# ============================================================",
    "# 8. ROI DIMENSIONS",
    "# ============================================================",
    "xMin = np.min(sectionPointsX)",
    "xMax = np.max(sectionPointsX)",
    "yMin = np.min(sectionPointsY)",
    "yMax = np.max(sectionPointsY)",
    "",
    "xCenter = (xMin + xMax) / 2.0",
    "yCenter = (yMin + yMax) / 2.0",
    "zCenter = (zLower + zUpper) / 2.0",
    "",
    "sizeX = (xMax - xMin) + 2.0 * TRANSVERSE_MARGIN_MM",
    "sizeY = (yMax - yMin) + 2.0 * TRANSVERSE_MARGIN_MM",
    "sizeZ = zUpper - zLower",
    "",
    "# ============================================================",
    "# 9. ROI CENTER IN RAS",
    "# ============================================================",
    "roiCenter = (",
    "    pointA",
    "    + xCenter * axisX",
    "    + yCenter * axisY",
    "    + zCenter * longMaxAxis",
    ")",
    "",
    "# ============================================================",
    "# 10. CREATE ROI",
    "# ============================================================",
    "roiName = slicer.mrmlScene.GenerateUniqueName(",
    "    modelNode.GetName() + '_ROI_' + ROI_LABEL",
    ")",
    "",
    "roiNode = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLMarkupsROINode', roiName)",
    "roiNode.CreateDefaultDisplayNodes()",
    "roiNode.SetSize([float(sizeX), float(sizeY), float(sizeZ)])",
    "",
    "# ============================================================",
    "# 11. ORIENT ROI: Z = LONGMAX",
    "# ============================================================",
    "roiToRAS = np.eye(4)",
    "roiToRAS[0:3, 0] = axisX",
    "roiToRAS[0:3, 1] = axisY",
    "roiToRAS[0:3, 2] = longMaxAxis",
    "roiToRAS[0:3, 3] = roiCenter",
    "",
    "roiMatrix = slicer.util.vtkMatrixFromArray(roiToRAS)",
    "roiNode.SetAndObserveObjectToNodeMatrix(roiMatrix)",
    "roiNode.GetDisplayNode().SetHandlesInteractive(True)",
    "",
    "# ============================================================",
    "# 12. OPTIONAL LONGMAX LINE",
    "# ============================================================",
    "if CREATE_LA_LINE:",
    "    lineName = slicer.mrmlScene.GenerateUniqueName(modelNode.GetName() + '_LongMax')",
    "    lineNode = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLMarkupsLineNode', lineName)",
    "    lineNode.CreateDefaultDisplayNodes()",
    "    lineNode.AddControlPointWorld(vtk.vtkVector3d(float(pointA[0]), float(pointA[1]), float(pointA[2])))",
    "    lineNode.AddControlPointWorld(vtk.vtkVector3d(float(pointB[0]), float(pointB[1]), float(pointB[2])))",
    "",
    "# ============================================================",
    "# 13. RESULTS",
    "# ============================================================",
    "print('\\n--------------------------------------------')",
    "print('ROI created:', roiNode.GetName())",
    "print('Longitudinal limits: %.3f mm (%.3f%%) - %.3f mm (%.3f%%)' % (zLower, LOWER_PERCENT, zUpper, UPPER_PERCENT))",
    "print('ROI length = %.3f mm' % sizeZ)",
    "print('ROI size = %.3f x %.3f x %.3f mm' % (sizeX, sizeY, sizeZ))",
    "print('ROI center = [%.3f, %.3f, %.3f]' % tuple(roiCenter))",
    "print('--------------------------------------------')",
    "print('DONE')",
    "print('--------------------------------------------\\n')",
    sep = "\n"
  )

  copy_to_clipboard(py)
}
