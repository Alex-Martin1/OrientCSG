# Internal 3D Slicer Python generator ----------------------------------------
#
# Convert a long-bone orientation result into one Python block per requested
# section. The generated block is intended to be pasted into the 3D Slicer
# Python Interactor with the corresponding mesh model already loaded.
emit_slicer_section_python <- function(res, section = NULL) {
  if (!res$type %in% c("TIBIA", "HUMERUS")) {
    stop("3D Slicer output is currently implemented only for `mode = \"TIBIA\"` or `mode = \"HUMERUS\"`.", call. = FALSE)
  }

  if (is.null(section)) {
    section <- names(res$section_points)[1]
  }

  if (!section %in% names(res$section_points)) {
    stop(
      sprintf(
        "Section '%s' does not exist. Available sections: %s",
        section,
        paste(names(res$section_points), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (identical(res$type, "TIBIA")) {
    if (is.null(res$projected$Proj_TibioTalar) || is.null(res$projected$Proj_Midpoint)) {
      stop("Projected tibial endpoints are required for Slicer output.", call. = FALSE)
    }
    distal_endpoint <- res$projected$Proj_TibioTalar
    proximal_endpoint <- res$projected$Proj_Midpoint
    anterior_up_sign <- -1
    ml_right_sign <- 1
  } else if (identical(res$type, "HUMERUS")) {
    if (is.null(res$projected$Proj_LM3) || is.null(res$projected$Proj_LM4)) {
      stop("Projected humeral endpoints are required for Slicer output.", call. = FALSE)
    }
    distal_endpoint <- res$projected$Proj_LM3
    proximal_endpoint <- res$projected$Proj_LM4
    anterior_up_sign <- 1
    ml_right_sign <- 1
  }

  # The R workflow stores coordinates in the external mesh/LPS convention. Slicer
  # expects RAS in the Python interactor, so X and Y are inverted here.
  Psec_ras <- flip_xy(res$section_points[[section]])
  L_ras <- flip_xy(res$vectors$L)
  ML_ras <- flip_xy(res$vectors$ML)
  AP_ras <- flip_xy(res$vectors$AP)
  distal_ras <- flip_xy(distal_endpoint)
  proximal_ras <- flip_xy(proximal_endpoint)

  section_percent <- sub("^SECTION_", "", section)

  model_name <- res$model_name
  if (is.null(model_name) || length(model_name) != 1L || is.na(model_name) || !nzchar(model_name)) {
    model_name <- "Segment_1_solid"
  }

  code <- c(
    "import slicer",
    "import vtk",
    "import numpy as np",
    "",
    "# ============================================================",
    "# OrientCSG Slicer section block generated from R",
    "# ============================================================",
    "",
    sprintf("MODEL_NAME = %s", py_quote(model_name)),
    "ALLOW_PARTIAL_MODEL_NAME_MATCH = True",
    "AUTO_SELECT_MODEL_IF_UNAMBIGUOUS = True",
    "",
    sprintf("SECTION_PERCENT = %s", fmt_num_py(as.numeric(section_percent))),
    sprintf("PSEC = %s", fmt_py_vec(Psec_ras)),
    sprintf("L = %s", fmt_py_vec(L_ras)),
    sprintf("ML = %s", fmt_py_vec(ML_ras)),
    sprintf("AP = %s", fmt_py_vec(AP_ras)),
    sprintf("DISTAL_AXIS_POINT = %s", fmt_py_vec(distal_ras)),
    sprintf("PROXIMAL_AXIS_POINT = %s", fmt_py_vec(proximal_ras)),
    "",
    sprintf("CAMERA_DISTANCE_MM = %s", fmt_num_py(res$camera_distance_mm)),
    "PARALLEL_SCALE_MARGIN = 0.60",
    "CREATE_FILLED_SECTION = True",
    "CREATE_SECTION_OUTLINE_NODE = False",
    "CREATE_AXIS_LINES = True",
    "L_AXIS_FULL_LENGTH = True",
    "ML_AP_AXIS_LENGTH_MM = 80.0",
    "CAPTURE_MODE = False",
    "HIDE_ORIGINAL_MODEL_IN_CAPTURE = True",
    sprintf("ANTERIOR_UP_SIGN = %s", fmt_num_py(anterior_up_sign)),
    sprintf("ML_RIGHT_SIGN = %s", fmt_num_py(ml_right_sign)),
    "RULER_COLOR = 0",
    "",
    "def nrm(v):",
    "    v = np.asarray(v, dtype=float)",
    "    s = np.linalg.norm(v)",
    "    if s < 1e-12:",
    "        raise ValueError('Near-zero vector cannot be normalized.')",
    "    return v / s",
    "",
    "def dot3(a, b):",
    "    return float(np.dot(a, b))",
    "",
    "def cross3(a, b):",
    "    return np.cross(a, b)",
    "",
    "def is_output_node(name):",
    "    return name.startswith('SECTION_') or name.startswith('OrientCSG') or name.startswith('Estimated_longitudinal_axis')",
    "",
    "def list_model_nodes():",
    "    print('\\nAvailable vtkMRMLModelNode objects:')",
    "    for i, node in enumerate(slicer.util.getNodesByClass('vtkMRMLModelNode')):",
    "        pd = node.GetPolyData()",
    "        n_points = pd.GetNumberOfPoints() if pd else 0",
    "        n_cells = pd.GetNumberOfCells() if pd else 0",
    "        print(f'  {i+1}. {node.GetName()} | points={n_points} | cells={n_cells}')",
    "",
    "def get_target_model_node():",
    "    plausible = []",
    "    for node in slicer.util.getNodesByClass('vtkMRMLModelNode'):",
    "        name = node.GetName()",
    "        pd = node.GetPolyData()",
    "        if pd is None or pd.GetNumberOfPoints() == 0 or pd.GetNumberOfCells() == 0:",
    "            continue",
    "        if is_output_node(name):",
    "            continue",
    "        plausible.append(node)",
    "    for node in plausible:",
    "        if node.GetName() == MODEL_NAME:",
    "            print(f'Using model node by exact name: \"{node.GetName()}\"')",
    "            return node",
    "    if ALLOW_PARTIAL_MODEL_NAME_MATCH:",
    "        matches = [node for node in plausible if MODEL_NAME.lower() in node.GetName().lower()]",
    "        if len(matches) == 1:",
    "            print(f'Using model node by partial match: \"{matches[0].GetName()}\"')",
    "            return matches[0]",
    "        if len(matches) > 1:",
    "            list_model_nodes()",
    "            raise ValueError('More than one model partially matches MODEL_NAME.')",
    "    if AUTO_SELECT_MODEL_IF_UNAMBIGUOUS and len(plausible) == 1:",
    "        print(f'Auto-selected only plausible model: \"{plausible[0].GetName()}\"')",
    "        return plausible[0]",
    "    list_model_nodes()",
    "    raise ValueError('Could not select target model. Set MODEL_NAME to the exact model name.')",
    "",
    "def clean_previous_outputs():",
    "    prefixes = [",
    "        f'SECTION_{SECTION_PERCENT:g}_outline',",
    "        f'SECTION_{SECTION_PERCENT:g}_filled',",
    "        f'SECTION_{SECTION_PERCENT:g}_L',",
    "        f'SECTION_{SECTION_PERCENT:g}_ML',",
    "        f'SECTION_{SECTION_PERCENT:g}_AP'",
    "    ]",
    "    to_remove = []",
    "    for cls in ['vtkMRMLModelNode', 'vtkMRMLMarkupsLineNode']:",
    "        for node in slicer.util.getNodesByClass(cls):",
    "            name = node.GetName()",
    "            if any(name.startswith(prefix) for prefix in prefixes):",
    "                to_remove.append(node)",
    "    for node in to_remove:",
    "        slicer.mrmlScene.RemoveNode(node)",
    "",
    "def get_model_polydata_world(modelNode):",
    "    polyData = modelNode.GetPolyData()",
    "    transformToWorld = vtk.vtkGeneralTransform()",
    "    slicer.vtkMRMLTransformNode.GetTransformBetweenNodes(modelNode.GetParentTransformNode(), None, transformToWorld)",
    "    tf = vtk.vtkTransformPolyDataFilter()",
    "    tf.SetInputData(polyData)",
    "    tf.SetTransform(transformToWorld)",
    "    tf.Update()",
    "    out = vtk.vtkPolyData()",
    "    out.DeepCopy(tf.GetOutput())",
    "    return out",
    "",
    "def create_section_polydata(polyData, Psec, normal):",
    "    plane = vtk.vtkPlane()",
    "    plane.SetOrigin(float(Psec[0]), float(Psec[1]), float(Psec[2]))",
    "    plane.SetNormal(float(normal[0]), float(normal[1]), float(normal[2]))",
    "    cutter = vtk.vtkCutter()",
    "    cutter.SetCutFunction(plane)",
    "    cutter.SetInputData(polyData)",
    "    cutter.Update()",
    "    clean = vtk.vtkCleanPolyData()",
    "    clean.SetInputData(cutter.GetOutput())",
    "    clean.Update()",
    "    stripper = vtk.vtkStripper()",
    "    stripper.SetInputData(clean.GetOutput())",
    "    stripper.Update()",
    "    out = vtk.vtkPolyData()",
    "    out.DeepCopy(stripper.GetOutput())",
    "    return out",
    "",
    "def triangulate_section(outlinePolyData):",
    "    triangulator = vtk.vtkContourTriangulator()",
    "    triangulator.SetInputData(outlinePolyData)",
    "    triangulator.Update()",
    "    filled = vtk.vtkPolyData()",
    "    filled.DeepCopy(triangulator.GetOutput())",
    "    return filled",
    "",
    "def add_model_node(polyData, name, color=(1.0, 1.0, 1.0), opacity=1.0, line_width=3.0):",
    "    node = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLModelNode', name)",
    "    node.SetAndObservePolyData(polyData)",
    "    node.CreateDefaultDisplayNodes()",
    "    d = node.GetDisplayNode()",
    "    d.SetColor(color)",
    "    d.SetOpacity(opacity)",
    "    d.SetLineWidth(line_width)",
    "    d.SetBackfaceCulling(0)",
    "    d.SetVisibility(True)",
    "    for method_name, value in [('SetLighting', False), ('SetInterpolation', 0), ('SetAmbient', 1.0), ('SetDiffuse', 0.0), ('SetSpecular', 0.0), ('SetPower', 1.0)]:",
    "        try:",
    "            getattr(d, method_name)(value)",
    "        except Exception:",
    "            pass",
    "    return node",
    "",
    "def add_axis_line(name, p1, p2, color=(1.0, 1.0, 1.0)):",
    "    lineNode = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLMarkupsLineNode', name)",
    "    lineNode.CreateDefaultDisplayNodes()",
    "    lineNode.AddControlPointWorld(vtk.vtkVector3d(float(p1[0]), float(p1[1]), float(p1[2])))",
    "    lineNode.AddControlPointWorld(vtk.vtkVector3d(float(p2[0]), float(p2[1]), float(p2[2])))",
    "    d = lineNode.GetDisplayNode()",
    "    d.SetSelectedColor(color)",
    "    d.SetLineThickness(0.35)",
    "    d.SetVisibility(True)",
    "    return lineNode",
    "",
    "def add_centered_axis_line(name, center, direction, length=80.0, color=(1.0, 1.0, 1.0)):",
    "    direction = nrm(direction)",
    "    p1 = center - 0.5 * length * direction",
    "    p2 = center + 0.5 * length * direction",
    "    return add_axis_line(name, p1, p2, color=color)",
    "",
    "def configure_view(threeDView):",
    "    viewNode = threeDView.mrmlViewNode()",
    "    viewNode.SetBackgroundColor(0.0, 0.0, 0.0)",
    "    viewNode.SetBackgroundColor2(0.0, 0.0, 0.0)",
    "    viewNode.SetBoxVisible(False)",
    "    viewNode.SetAxisLabelsVisible(False)",
    "    viewNode.SetOrientationMarkerType(0)",
    "    viewNode.SetRulerType(1)",
    "    try:",
    "        viewNode.SetRulerColor(int(RULER_COLOR))",
    "    except Exception:",
    "        pass",
    "    try:",
    "        viewNode.SetShadowsVisibility(False)",
    "    except Exception:",
    "        pass",
    "    try:",
    "        viewNode.SetRenderMode(viewNode.Orthographic)",
    "    except Exception:",
    "        pass",
    "    viewNode.Modified()",
    "",
    "def widget_size(widget):",
    "    try:",
    "        return int(widget.width), int(widget.height)",
    "    except Exception:",
    "        return int(widget.width()), int(widget.height())",
    "",
    "def compute_camera_basis(L, ML, AP):",
    "    Zcam = nrm(L)",
    "    if dot3(Zcam, np.array([0.0, 0.0, -1.0])) < 0:",
    "        Zcam = -Zcam",
    "    desired_up = ANTERIOR_UP_SIGN * AP",
    "    desired_up = desired_up - dot3(desired_up, Zcam) * Zcam",
    "    Ycam = nrm(desired_up)",
    "    desired_right = ML_RIGHT_SIGN * ML",
    "    desired_right = desired_right - dot3(desired_right, Zcam) * Zcam",
    "    desired_right = desired_right - dot3(desired_right, Ycam) * Ycam",
    "    if np.linalg.norm(desired_right) < 1e-12:",
    "        Xcam = nrm(cross3(Ycam, Zcam))",
    "    else:",
    "        Xcam = nrm(desired_right)",
    "    Ycam = nrm(cross3(Zcam, Xcam))",
    "    if dot3(Ycam, desired_up) < 0:",
    "        Xcam = -Xcam",
    "        Ycam = -Ycam",
    "    return Xcam, Ycam, Zcam",
    "",
    "def set_camera(Psec, Xcam, Ycam, Zcam, sectionPolyData):",
    "    threeDView = slicer.app.layoutManager().threeDWidget(0).threeDView()",
    "    cameraNode = threeDView.cameraNode()",
    "    camera = cameraNode.GetCamera()",
    "    configure_view(threeDView)",
    "    C = Psec + CAMERA_DISTANCE_MM * Zcam",
    "    camera.SetPosition(float(C[0]), float(C[1]), float(C[2]))",
    "    camera.SetFocalPoint(float(Psec[0]), float(Psec[1]), float(Psec[2]))",
    "    camera.SetViewUp(float(Ycam[0]), float(Ycam[1]), float(Ycam[2]))",
    "    camera.ParallelProjectionOn()",
    "    camera.OrthogonalizeViewUp()",
    "    if sectionPolyData is not None and sectionPolyData.GetNumberOfPoints() > 0:",
    "        pts = sectionPolyData.GetPoints()",
    "        coords = np.array([pts.GetPoint(i) for i in range(pts.GetNumberOfPoints())], dtype=float)",
    "        rel = coords - Psec",
    "        x = rel @ Xcam",
    "        y = rel @ Ycam",
    "        span_x = max(1.0, float(np.max(x) - np.min(x)))",
    "        span_y = max(1.0, float(np.max(y) - np.min(y)))",
    "        width, height = widget_size(threeDView)",
    "        aspect = max(1.0, width / max(1, height))",
    "        camera.SetParallelScale(PARALLEL_SCALE_MARGIN * max(span_y, span_x / aspect))",
    "    else:",
    "        camera.SetParallelScale(40.0)",
    "    camera.SetClippingRange(1.0, 5000.0)",
    "    cameraNode.Modified()",
    "    threeDView.forceRender()",
    "    return camera",
    "",
    "def save_camera_state(modelNode, camera):",
    "    modelNode.SetAttribute('OrientCSG.CameraPosition', ','.join([f'{v:.12g}' for v in camera.GetPosition()]))",
    "    modelNode.SetAttribute('OrientCSG.CameraFocalPoint', ','.join([f'{v:.12g}' for v in camera.GetFocalPoint()]))",
    "    modelNode.SetAttribute('OrientCSG.CameraViewUp', ','.join([f'{v:.12g}' for v in camera.GetViewUp()]))",
    "    modelNode.SetAttribute('OrientCSG.CameraParallelScale', f'{camera.GetParallelScale():.12g}')",
    "",
    "def restore_orientcsg_camera_state(modelNode=None):",
    "    if modelNode is None:",
    "        modelNode = get_target_model_node()",
    "    threeDView = slicer.app.layoutManager().threeDWidget(0).threeDView()",
    "    cameraNode = threeDView.cameraNode()",
    "    camera = cameraNode.GetCamera()",
    "    pos_txt = modelNode.GetAttribute('OrientCSG.CameraPosition')",
    "    foc_txt = modelNode.GetAttribute('OrientCSG.CameraFocalPoint')",
    "    up_txt = modelNode.GetAttribute('OrientCSG.CameraViewUp')",
    "    scale_txt = modelNode.GetAttribute('OrientCSG.CameraParallelScale')",
    "    if None in [pos_txt, foc_txt, up_txt, scale_txt]:",
    "        raise ValueError('No saved OrientCSG camera state found.')",
    "    pos = [float(x) for x in pos_txt.split(',')]",
    "    foc = [float(x) for x in foc_txt.split(',')]",
    "    up = [float(x) for x in up_txt.split(',')]",
    "    scale = float(scale_txt)",
    "    configure_view(threeDView)",
    "    camera.SetPosition(*pos)",
    "    camera.SetFocalPoint(*foc)",
    "    camera.SetViewUp(*up)",
    "    camera.SetParallelScale(scale)",
    "    camera.ParallelProjectionOn()",
    "    camera.OrthogonalizeViewUp()",
    "    cameraNode.Modified()",
    "    threeDView.forceRender()",
    "",
    "clean_previous_outputs()",
    "modelNode = get_target_model_node()",
    "modelNode.CreateDefaultDisplayNodes()",
    "polyDataWorld = get_model_polydata_world(modelNode)",
    "sectionOutline = create_section_polydata(polyDataWorld, PSEC, L)",
    "if sectionOutline.GetNumberOfPoints() == 0:",
    "    raise ValueError('The section plane did not intersect the model.')",
    "sectionFilledNode = None",
    "if CREATE_FILLED_SECTION:",
    "    sectionFilled = triangulate_section(sectionOutline)",
    "    if sectionFilled.GetNumberOfCells() > 0:",
    "        sectionFilledNode = add_model_node(sectionFilled, name=f'SECTION_{SECTION_PERCENT:g}_filled', color=(1.0, 1.0, 1.0), opacity=1.0, line_width=1.0)",
    "    else:",
    "        print('WARNING: Could not triangulate filled section.')",
    "sectionOutlineNode = None",
    "if CREATE_SECTION_OUTLINE_NODE:",
    "    sectionOutlineNode = add_model_node(sectionOutline, name=f'SECTION_{SECTION_PERCENT:g}_outline', color=(1.0, 1.0, 1.0), opacity=1.0, line_width=4.0)",
    "if CREATE_AXIS_LINES:",
    "    if L_AXIS_FULL_LENGTH:",
    "        add_axis_line(f'SECTION_{SECTION_PERCENT:g}_L', DISTAL_AXIS_POINT, PROXIMAL_AXIS_POINT, color=(0.0, 1.0, 0.0))",
    "    else:",
    "        add_centered_axis_line(f'SECTION_{SECTION_PERCENT:g}_L', PSEC, L, length=np.linalg.norm(PROXIMAL_AXIS_POINT - DISTAL_AXIS_POINT), color=(0.0, 1.0, 0.0))",
    "    add_centered_axis_line(f'SECTION_{SECTION_PERCENT:g}_ML', PSEC, ML, length=ML_AP_AXIS_LENGTH_MM, color=(0.0, 0.4, 1.0))",
    "    add_centered_axis_line(f'SECTION_{SECTION_PERCENT:g}_AP', PSEC, AP, length=ML_AP_AXIS_LENGTH_MM, color=(1.0, 0.0, 0.0))",
    "d = modelNode.GetDisplayNode()",
    "if CAPTURE_MODE and HIDE_ORIGINAL_MODEL_IN_CAPTURE:",
    "    d.SetVisibility(False)",
    "else:",
    "    d.SetVisibility(True)",
    "    d.SetOpacity(0.22)",
    "Xcam, Ycam, Zcam = compute_camera_basis(L, ML, AP)",
    "camera = set_camera(PSEC, Xcam, Ycam, Zcam, sectionOutline)",
    "save_camera_state(modelNode, camera)",
    "",
    "print('\\n================ ORIENTCSG / SLICER SECTION ================\\n')",
    "print('Model:', modelNode.GetName())",
    "print('Section percent:', SECTION_PERCENT)",
    "print('\\nPSEC, Slicer RAS:')",
    "print(PSEC)",
    "print('\\nL, ML, AP, Slicer RAS:')",
    "print(L)",
    "print(ML)",
    "print(AP)",
    "print('\\nCreated model nodes:')",
    "if sectionFilledNode is not None:",
    "    print(sectionFilledNode.GetName())",
    "if sectionOutlineNode is not None:",
    "    print(sectionOutlineNode.GetName())",
    "else:",
    "    print('No outline model node was created.')",
    "print('\\nAxis lines: L green, ML blue, AP red')",
    "print('Ruler visible, requested color enum:', RULER_COLOR)",
    "print('To restore this view later, run: restore_orientcsg_camera_state()')"
  )

  paste(code, collapse = "\n")
}

# Internal 3D Slicer Python generator for mandibles --------------------------
#
# Convert a mandibular orientation result into one Python block per planned
# section. The generated code orients a Slicer slice view to the same section
# plane used by the Avizo/Amira backend. Internally OrientCSG stores all points
# and vectors in LPS/external coordinates; the Python block receives RAS values.
emit_slicer_mandible_python <- function(res, section = NULL) {
  available_sections <- c("CS1", "CS2", "CS3")

  if (is.null(section)) {
    section <- available_sections[1]
  }

  section <- toupper(trimws(section))
  if (!section %in% available_sections) {
    stop(
      sprintf(
        "Section '%s' does not exist. Available sections: %s",
        section,
        paste(available_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  LM <- res$landmarks
  LM1 <- LM["LM1", ]
  LM2 <- LM["LM2", ]
  LM1_Line <- res$points$LM1_Line

  if (section == "CS1") {
    Psec <- res$points$CS1B
    normal <- res$vectors$Vec_CS1_Normal
    x_ref <- project_vector_to_plane(res$vectors$Vec_CS1, normal)
  } else if (section == "CS2") {
    Psec <- res$points$CS2B
    normal <- res$vectors$Vec_CS2_Normal
    x_ref <- project_vector_to_plane(res$vectors$Vec_CS2, normal)
  } else {
    Psec <- LM2
    normal <- res$vectors$Vec_1_1Line
    x_ref <- project_vector_to_plane(res$vectors$Vec_0_2, normal)
  }

  if (sqrt(sum(x_ref^2)) < 1e-12) {
    stop(sprintf("%s: screen-horizontal reference vector cannot be defined.", section), call. = FALSE)
  }

  Psec_ras <- flip_xy(Psec)
  normal_ras <- flip_xy(normal)
  x_ref_ras <- flip_xy(x_ref)
  y_pref_ras <- flip_xy(res$vectors$Vec_Penp)
  arp_origin_ras <- flip_xy(res$points$ARP_Origin)
  arp_normal_ras <- flip_xy(res$vectors$Vec_Penp)
  lm1_ras <- flip_xy(LM1)
  lm2_ras <- flip_xy(LM2)
  lm1_line_ras <- flip_xy(LM1_Line)

  volume_name <- res$volume_name
  if (is.null(volume_name) || length(volume_name) != 1L || is.na(volume_name) || !nzchar(volume_name)) {
    volume_name <- ""
  }

  code <- c(
    "import slicer",
    "import vtk",
    "import numpy as np",
    "",
    "# ============================================================",
    "# OrientCSG Slicer mandibular slice block generated from R",
    "# ============================================================",
    "",
    sprintf("SECTION_LABEL = %s", py_quote(section)),
    sprintf("VOLUME_NAME = %s", py_quote(volume_name)),
    "SLICE_VIEW_NAME = 'Red'",
    "FIT_SLICE_TO_VOLUME = True",
    "SHOW_SLICE_INTERSECTIONS = True",
    "CREATE_ARP_MARKUPS_PLANE = False",
    "ARP_MARKUPS_PLANE_SIZE_MM = 80.0",
    "",
    sprintf("PSEC = %s", fmt_py_vec(Psec_ras)),
    sprintf("NORMAL = %s", fmt_py_vec(normal_ras)),
    sprintf("X_SCREEN_REFERENCE = %s", fmt_py_vec(x_ref_ras)),
    sprintf("Y_PREFERRED = %s", fmt_py_vec(y_pref_ras)),
    sprintf("ARP_ORIGIN = %s", fmt_py_vec(arp_origin_ras)),
    sprintf("ARP_NORMAL = %s", fmt_py_vec(arp_normal_ras)),
    sprintf("LM1 = %s", fmt_py_vec(lm1_ras)),
    sprintf("LM2 = %s", fmt_py_vec(lm2_ras)),
    sprintf("LM1_LINE = %s", fmt_py_vec(lm1_line_ras)),
    "",
    "def nrm(v):",
    "    v = np.asarray(v, dtype=float)",
    "    s = np.linalg.norm(v)",
    "    if s < 1e-12:",
    "        raise ValueError('Near-zero vector cannot be normalized.')",
    "    return v / s",
    "",
    "def dot3(a, b):",
    "    return float(np.dot(a, b))",
    "",
    "def make_slice_to_ras(point, normal, x_reference, y_preferred):",
    "    point = np.asarray(point, dtype=float)",
    "    z_axis = nrm(normal)",
    "    x_axis = np.asarray(x_reference, dtype=float) - dot3(x_reference, z_axis) * z_axis",
    "    if np.linalg.norm(x_axis) < 1e-12:",
    "        y_tmp = np.asarray(y_preferred, dtype=float) - dot3(y_preferred, z_axis) * z_axis",
    "        if np.linalg.norm(y_tmp) < 1e-12:",
    "            raise ValueError('Cannot construct slice axes: both reference vectors are collinear with the normal.')",
    "        x_axis = np.cross(y_tmp, z_axis)",
    "    x_axis = nrm(x_axis)",
    "    y_axis = nrm(np.cross(z_axis, x_axis))",
    "    x_axis = nrm(np.cross(y_axis, z_axis))",
    "    y_pref = np.asarray(y_preferred, dtype=float) - dot3(y_preferred, z_axis) * z_axis",
    "    if np.linalg.norm(y_pref) > 1e-12 and dot3(y_axis, nrm(y_pref)) < 0:",
    "        x_axis = -x_axis",
    "        y_axis = -y_axis",
    "    m = vtk.vtkMatrix4x4()",
    "    m.Identity()",
    "    for i in range(3):",
    "        m.SetElement(i, 0, float(x_axis[i]))",
    "        m.SetElement(i, 1, float(y_axis[i]))",
    "        m.SetElement(i, 2, float(z_axis[i]))",
    "        m.SetElement(i, 3, float(point[i]))",
    "    return m, x_axis, y_axis, z_axis",
    "",
    "def get_slice_widget_and_logic():",
    "    lm = slicer.app.layoutManager()",
    "    widget = lm.sliceWidget(SLICE_VIEW_NAME)",
    "    if widget is None:",
    "        raise ValueError(f'Could not find Slicer slice view: {SLICE_VIEW_NAME}')",
    "    return widget, widget.sliceLogic()",
    "",
    "def get_volume_node(sliceLogic):",
    "    if VOLUME_NAME:",
    "        node = slicer.util.getFirstNodeByName(VOLUME_NAME)",
    "        if node is None:",
    "            raise ValueError(f'Could not find volume node named: {VOLUME_NAME}')",
    "        return node",
    "    comp = sliceLogic.GetSliceCompositeNode()",
    "    if comp is not None and comp.GetBackgroundVolumeID():",
    "        node = slicer.mrmlScene.GetNodeByID(comp.GetBackgroundVolumeID())",
    "        if node is not None:",
    "            return node",
    "    nodes = slicer.util.getNodesByClass('vtkMRMLScalarVolumeNode')",
    "    if len(nodes) == 1:",
    "        return nodes[0]",
    "    if len(nodes) > 1:",
    "        print('Available scalar volume nodes:')",
    "        for node in nodes:",
    "            print('  ', node.GetName())",
    "        raise ValueError('More than one scalar volume exists. Set VOLUME_NAME explicitly.')",
    "    raise ValueError('No scalar volume node found. Load the DICOM/volume before running this block.')",
    "",
    "def set_background_volume(sliceLogic, volumeNode):",
    "    comp = sliceLogic.GetSliceCompositeNode()",
    "    if comp is not None:",
    "        comp.SetBackgroundVolumeID(volumeNode.GetID())",
    "",
    "def create_arp_markups_plane():",
    "    try:",
    "        node = slicer.mrmlScene.AddNewNodeByClass('vtkMRMLMarkupsPlaneNode', 'OrientCSG_ARP')",
    "        node.SetOriginWorld(ARP_ORIGIN)",
    "        node.SetNormalWorld(ARP_NORMAL)",
    "        node.SetSizeWorld([ARP_MARKUPS_PLANE_SIZE_MM, ARP_MARKUPS_PLANE_SIZE_MM])",
    "        node.CreateDefaultDisplayNodes()",
    "        d = node.GetDisplayNode()",
    "        if d is not None:",
    "            d.SetColor(0.0, 1.0, 0.0)",
    "            d.SetOpacity(0.35)",
    "        return node",
    "    except Exception as exc:",
    "        print('WARNING: could not create ARP Markups plane:', exc)",
    "        return None",
    "",
    "sliceWidget, sliceLogic = get_slice_widget_and_logic()",
    "sliceNode = sliceLogic.GetSliceNode()",
    "volumeNode = get_volume_node(sliceLogic)",
    "set_background_volume(sliceLogic, volumeNode)",
    "sliceToRAS, X_AXIS, Y_AXIS, Z_AXIS = make_slice_to_ras(PSEC, NORMAL, X_SCREEN_REFERENCE, Y_PREFERRED)",
    "sliceNode.SetSliceToRAS(sliceToRAS)",
    "sliceNode.UpdateMatrices()",
    "sliceNode.SetSliceVisible(True)",
    "sliceNode.SetWidgetVisible(True)",
    "if SHOW_SLICE_INTERSECTIONS:",
    "    try:",
    "        sliceNode.SetSliceEdgeVisibility3D(True)",
    "    except Exception:",
    "        pass",
    "if FIT_SLICE_TO_VOLUME:",
    "    sliceLogic.FitSliceToAll()",
    "if CREATE_ARP_MARKUPS_PLANE:",
    "    create_arp_markups_plane()",
    "sliceWidget.sliceView().scheduleRender()",
    "",
    "print('================ ORIENTCSG / SLICER MANDIBLE ================')",
    "print('Section:', SECTION_LABEL)",
    "print('Volume:', volumeNode.GetName())",
    "print('Slice view:', SLICE_VIEW_NAME)",
    "print('PSEC, Slicer RAS:', PSEC)",
    "print('NORMAL, Slicer RAS:', NORMAL)",
    "print('Screen X, Y and normal axes, Slicer RAS:')",
    "print(X_AXIS)",
    "print(Y_AXIS)",
    "print(Z_AXIS)",
    "print('ARP origin and normal, Slicer RAS:')",
    "print(ARP_ORIGIN)",
    "print(ARP_NORMAL)"
  )

  paste(code, collapse = "\n")
}
