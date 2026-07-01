# Installation, if OrientCSG is not installed yet:

# install.packages("remotes")
# remotes::install_github("Alex-Martin1/OrientCSG")

library(OrientCSG)

# Notes on example format -------------------------------------------------
#
# The TIBIA and HUMERUS examples use the pre-June 2026 BoneJ output format,
# where the longitudinal-axis information is pasted as a compact 3 x 3 matrix.
# The FEMUR and RADIUS examples use the post-June 2026 BoneJ output format,
# where the same information is pasted from the updated tabular output row.
# OrientCSG accepts both formats; the mixed examples are intentional and show
# backward compatibility across the BoneJ output update.
#
# In the 3D Slicer workflows, landmarks copied from the Markups table are shown
# in table format. These examples use lm_coord_system = "LPS", matching the
# coordinate convention expected by the workflow when coordinates are copied
# from the Slicer Markups table rather than extracted as RAS world coordinates.


# 1. TIBIA example ===========================================================
#
# Landmark order for mode = "TIBIA":
#
#   LM1 = tibial plateau landmark 1
#   LM2 = tibial plateau landmark 2
#   LM3 = tibio-talar landmark
#
# Biomechanical length is measured as the projected distance along the tibial
# longitudinal axis from LM3 to the midpoint between LM1 and LM2.

dicom_iop_str_tibia <- r"(0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0)"

longitudinal_matrix_str_tibia <- "
||0.011|-0.780|-0.626||
||0.019|-0.626|0.780||
||1.000|0.021|-0.008||
"

tibia_landmarks_str_T108_Left_A <- "
130.94606  -12.514749 -392.244507
164.351898 -17.573267 -395.017944
146.258621 -15.388991  -61.599937
"

res_tibia <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str_tibia,
  dicom_iop = dicom_iop_str_tibia,
  landmarks_str = tibia_landmarks_str_T108_Left_A,
  section_loc = 50,
  individual_id = "T108_Left",
  camera_distance_mm = 300
)

res_tibia$summary
#View(res_tibia$summary)
res_tibia$manual_orientation

cat(get_tcl(res_tibia, section = "SECTION_50"))

# To copy this command block to the clipboard, run:
copy_tcl(res_tibia, section = "SECTION_50")


# 1B. TIBIA: CT/DICOM true cross-section + 3D Slicer workflow ================
#
# Same specimen and section location as in the main TIBIA example above, but
# with the landmarks represented as if copied from the 3D Slicer Markups table.

tibia_landmarks_slicer_T108_Left_B <- "
1 130.946060 -12.514749 -392.244507 0 0 0 1 1 1 0 F-1 2 0
2 164.351898 -17.573267 -395.017944 0 0 0 1 1 1 0 F-2 2 0
3 146.258621 -15.388991  -61.599937 0 0 0 1 1 1 0 F-3 2 0
"

res_tibia_true_slicer <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str_tibia,
  dicom_iop = dicom_iop_str_tibia,
  landmarks_str = tibia_landmarks_slicer_T108_Left_B,
  section_loc = 50,
  individual_id = "T108_Left",
  SOLID = FALSE,
  SLICER = TRUE,
  USE_ANAT_ORIENT = TRUE,
  volume_name = "T108_Left_DICOM_VOLUME",
  camera_distance_mm = 300,
  lm_coord_system = "LPS"
)

res_tibia_true_slicer$summary
#View(res_tibia_true_slicer$summary)

cat(get_slicer_py(res_tibia_true_slicer, section = "SECTION_50"))

# To copy this Python block to the clipboard, run:
copy_slicer_py(res_tibia_true_slicer, section = "SECTION_50")


# 1C. TIBIA: solid mesh + 3D Slicer workflow =================================
#
# This workflow is intended for closed surface meshes (.ply, .stl, .obj).
# Replace mesh_file_tibia with the full path to your own watertight mesh file.

mesh_file_tibia <- "C:/Users/Alex/Desktop/T108_Left_solid.ply"

tibia_landmarks_slicer_T108_Left_C <- "
1 130.946060 -12.514749 -392.244507 0 0 0 1 1 1 0 F-1 2 0
2 164.351898 -17.573267 -395.017944 0 0 0 1 1 1 0 F-2 2 0
3 146.258621 -15.388991  -61.599937 0 0 0 1 1 1 0 F-3 2 0
"

if (file.exists(mesh_file_tibia)) {
  res_tibia_solid_slicer <- orient_longbone(
    mode = "TIBIA",
    mesh_file = mesh_file_tibia,
    landmarks_str = tibia_landmarks_slicer_T108_Left_C,
    lm_coord_system = "LPS",
    section_loc = 50,
    individual_id = "T108_Left",
    model_name = "T108_Left_solid",
    SOLID = TRUE,
    SLICER = TRUE,
    USE_ANAT_ORIENT = TRUE,
    camera_distance_mm = 300
  )
  
  res_tibia_solid_slicer$summary
  #View(res_tibia_solid_slicer$summary)
  res_tibia_solid_slicer$mesh_axes$eigenvectors
  
  cat(get_slicer_py(res_tibia_solid_slicer, section = "SECTION_50"))
  
  copy_slicer_py(res_tibia_solid_slicer, section = "SECTION_50")
} else {
  message("Skipping 1C: tibial mesh file not found. Edit mesh_file_tibia to run this example.")
}


# 2. HUMERUS example =========================================================
#
# Landmark order for mode = "HUMERUS":
#
#   LM1 = distal landmark used to define the mediolateral reference direction
#   LM2 = second distal landmark used to define the mediolateral reference direction
#   LM3 = distal landmark for biomechanical length
#   LM4 = proximal landmark for biomechanical length
#
# Biomechanical length is measured as the projected distance along the humeral
# longitudinal axis from LM3 to LM4.

dicom_iop_str_humerus <- r"(0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0)"

longitudinal_matrix_str_humerus <- "
||0.023|0.973|0.231||
||0.022|-0.232|0.972||
||0.999|-0.018|-0.027||
"

humerus_landmarks_str_H108_Right_A <- "
1.647897491455078e+002 -1.567003917694092e+001 -6.820565032958984e+001
1.863933868408203e+002 -1.576045989990234e+001 -6.810215759277344e+001
182.2418 -6.976971 -59.92139
182.7214  8.127365 -345.48276
"

res_humerus <- orient_longbone(
  mode = "HUMERUS",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  dicom_iop = dicom_iop_str_humerus,
  landmarks_str = humerus_landmarks_str_H108_Right_A,
  section_loc = c(35, 50),
  individual_id = "H108_Right",
  camera_distance_mm = 300
)

res_humerus$summary
#View(res_humerus$summary)
res_humerus$manual_orientation

cat(get_tcl(res_humerus, section = "SECTION_35"))
cat(get_tcl(res_humerus, section = "SECTION_50"))

copy_tcl(res_humerus, section = "SECTION_35")
copy_tcl(res_humerus, section = "SECTION_50")


# 2B. HUMERUS: CT/DICOM true cross-section + 3D Slicer workflow ==============
#
# Same specimen and section locations as in the main HUMERUS example above, but
# with the landmarks represented as if copied from the 3D Slicer Markups table.

humerus_landmarks_slicer_H108_Right_B <- "
1 164.789749145508 -15.670039176941 -68.205650329590 0 0 0 1 1 1 0 F-1 2 0
2 186.393386840820 -15.760459899902 -68.102157592773 0 0 0 1 1 1 0 F-2 2 0
3 182.241800000000  -6.976971000000 -59.921390000000 0 0 0 1 1 1 0 F-3 2 0
4 182.721400000000   8.127365000000 -345.482760000000 0 0 0 1 1 1 0 F-4 2 0
"

res_humerus_true_slicer <- orient_longbone(
  mode = "HUMERUS",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  dicom_iop = dicom_iop_str_humerus,
  landmarks_str = humerus_landmarks_slicer_H108_Right_B,
  section_loc = c(35, 50),
  individual_id = "H108_Right",
  SOLID = FALSE,
  SLICER = TRUE,
  USE_ANAT_ORIENT = TRUE,
  volume_name = "H108_Right_DICOM_VOLUME",
  camera_distance_mm = 300,
  lm_coord_system = "LPS"
)

res_humerus_true_slicer$summary
#View(res_humerus_true_slicer$summary)

cat(get_slicer_py(res_humerus_true_slicer, section = "SECTION_35"))
cat(get_slicer_py(res_humerus_true_slicer, section = "SECTION_50"))

copy_slicer_py(res_humerus_true_slicer, section = "SECTION_35")
copy_slicer_py(res_humerus_true_slicer, section = "SECTION_50")


# 2C. HUMERUS: solid mesh + 3D Slicer workflow ===============================
#
# Replace mesh_file_humerus with the full path to your own watertight mesh file.

mesh_file_humerus <- "C:/Users/Alex/Desktop/H108_Right_solid.ply"

humerus_landmarks_slicer_H108_Right_C <- "
1 164.789749145508 -15.670039176941 -68.205650329590 0 0 0 1 1 1 0 F-1 2 0
2 186.393386840820 -15.760459899902 -68.102157592773 0 0 0 1 1 1 0 F-2 2 0
3 182.241800000000  -6.976971000000 -59.921390000000 0 0 0 1 1 1 0 F-3 2 0
4 182.721400000000   8.127365000000 -345.482760000000 0 0 0 1 1 1 0 F-4 2 0
"

if (file.exists(mesh_file_humerus)) {
  res_humerus_solid_slicer <- orient_longbone(
    mode = "HUMERUS",
    mesh_file = mesh_file_humerus,
    landmarks_str = humerus_landmarks_slicer_H108_Right_C,
    lm_coord_system = "LPS",
    section_loc = c(35, 50),
    individual_id = "H108_Right",
    model_name = "H108_Right_solid",
    SOLID = TRUE,
    SLICER = TRUE,
    USE_ANAT_ORIENT = TRUE,
    camera_distance_mm = 300
  )
  
  res_humerus_solid_slicer$summary
  #View(res_humerus_solid_slicer$summary)
  res_humerus_solid_slicer$mesh_axes$eigenvectors
  
  cat(get_slicer_py(res_humerus_solid_slicer, section = "SECTION_35"))
  cat(get_slicer_py(res_humerus_solid_slicer, section = "SECTION_50"))
  
  copy_slicer_py(res_humerus_solid_slicer, section = "SECTION_35")
  copy_slicer_py(res_humerus_solid_slicer, section = "SECTION_50")
} else {
  message("Skipping 2C: humeral mesh file not found. Edit mesh_file_humerus to run this example.")
}


# 3. FEMUR example ===========================================================
#
# Landmark order for mode = "FEMUR":
#
#   LM1 = Condyle1, geometric centre of one distal condylar articular surface
#   LM2 = Condyle2, geometric centre of the opposite distal condylar articular surface
#   LM3 = SuperiorNeck, deepest and most distal point on the superior femoral-neck surface
#
# Biomechanical length is measured as the projected distance along the femoral
# longitudinal axis from the distal condylar midpoint to LM3.

dicom_iop_str_femur <- r"(0020,0037  Image Orientation (Patient): -1\0\0\0\-1\0)"

longitudinal_matrix_str_femur <- "
AAM_T-324_fémur_I	57.46085720770676	74.30795885498785	253.907737301091	137317.32610560616	247.17118699005323	1691827.0417617331	1690769.8657119467	50641.81658958559	3065.504282644045	-39612.15593432332	-81494.7808522638	1695292.242516427	1692302.1959205428	45644.285626294324	-0.02411871280310699	-0.04950688973444657	0.9984825264177349	-0.9154090025393372	-0.4003318665483994	-0.041961347640542476	-0.4018017493275227	0.9150319472535892	0.035663563235422066
"

femur_landmarks_str_F324_Left_A <- "
-1.771405487060547e+002 1.268690013885498e+001 -2.896743774414063e+001
-1.273115997314453e+002 1.260385799407959e+001 -3.878551864624023e+001
-1.647595825195313e+002 1.156296730041504e+001 -4.749829101562500e+002
"

res_femur <- orient_longbone(
  mode = "FEMUR",
  longitudinal_matrix_str = longitudinal_matrix_str_femur,
  dicom_iop = dicom_iop_str_femur,
  landmarks_str = femur_landmarks_str_F324_Left_A,
  section_loc = 50,
  individual_id = "F324_Left",
  camera_distance_mm = 300
)

res_femur$summary
#View(res_femur$summary)
res_femur$manual_orientation

cat(get_tcl(res_femur, section = "SECTION_50"))

copy_tcl(res_femur, section = "SECTION_50")


# 3B. FEMUR: CT/DICOM true cross-section + 3D Slicer workflow ================
#
# Same specimen and section location as in the main FEMUR example above, but
# with the landmarks represented as if copied from the 3D Slicer Markups table.

femur_landmarks_slicer_F324_Left_B <- "
1 -177.140548706055 12.686900138855  -28.967437744141 0 0 0 1 1 1 0 F-1 2 0
2 -127.311599731445 12.603857994080  -38.785518646240 0 0 0 1 1 1 0 F-2 2 0
3 -164.759582519531 11.562967300415 -474.982910156250 0 0 0 1 1 1 0 F-3 2 0
"

res_femur_true_slicer <- orient_longbone(
  mode = "FEMUR",
  longitudinal_matrix_str = longitudinal_matrix_str_femur,
  dicom_iop = dicom_iop_str_femur,
  landmarks_str = femur_landmarks_slicer_F324_Left_B,
  section_loc = 50,
  individual_id = "F324_Left",
  SOLID = FALSE,
  SLICER = TRUE,
  USE_ANAT_ORIENT = TRUE,
  volume_name = "AAM_T-324_fémur_I",
  camera_distance_mm = 300,
  lm_coord_system = "LPS"
)

res_femur_true_slicer$summary
#View(res_femur_true_slicer$summary)

cat(get_slicer_py(res_femur_true_slicer, section = "SECTION_50"))

copy_slicer_py(res_femur_true_slicer, section = "SECTION_50")


# 3C. FEMUR: solid mesh + 3D Slicer workflow =================================
#
# Replace mesh_file_femur with the full path to the corresponding closed
# surface mesh. The directory is illustrative; the file name is kept
# consistent with the femoral example above.

mesh_file_femur <- "C:/Users/Alex/Desktop/AAM_T-324_fémur_I.ply"

femur_landmarks_slicer_F324_Left_C <- "
1 -177.140548706055 12.686900138855  -28.967437744141 0 0 0 1 1 1 0 F-1 2 0
2 -127.311599731445 12.603857994080  -38.785518646240 0 0 0 1 1 1 0 F-2 2 0
3 -164.759582519531 11.562967300415 -474.982910156250 0 0 0 1 1 1 0 F-3 2 0
"

if (file.exists(mesh_file_femur)) {
  res_femur_solid_slicer <- orient_longbone(
    mode = "FEMUR",
    mesh_file = mesh_file_femur,
    landmarks_str = femur_landmarks_slicer_F324_Left_C,
    lm_coord_system = "LPS",
    section_loc = 50,
    individual_id = "F324_Left",
    model_name = "AAM_T-324_fémur_I",
    SOLID = TRUE,
    SLICER = TRUE,
    USE_ANAT_ORIENT = TRUE,
    camera_distance_mm = 300
  )
  
  res_femur_solid_slicer$summary
  #View(res_femur_solid_slicer$summary)
  res_femur_solid_slicer$mesh_axes$eigenvectors
  
  cat(get_slicer_py(res_femur_solid_slicer, section = "SECTION_50"))
  
  copy_slicer_py(res_femur_solid_slicer, section = "SECTION_50")
} else {
  message("Skipping 3C: femoral mesh file not found. Edit mesh_file_femur to run this example.")
}


# 4. RADIUS example ==========================================================
#
# Landmark order for mode = "RADIUS":
#
#   LM1 = RadialStyloid, most lateral point on the radial styloid tip
#   LM2 = UlnarNotch, midpoint of the ulnar notch border
#   LM3 = DistArticular, geometric centre of the distal radiocarpal surface
#   LM4 = ProxArticular, geometric centre of the proximal radial-head surface
#
# Biomechanical length is measured as the projected distance along the radial
# longitudinal axis from LM3 to LM4.

dicom_iop_str_radius <- r"(0020,0037  Image Orientation (Patient): -1\0\0\0\-1\0)"

longitudinal_matrix_str_radius <- "
AAM_T-324_radio_D	44.080015942418335	42.95720179215419	133.55952713852332	27089.50882590009	48.761115886621546	327980.4273665555	329095.261157972	3115.9014055120465	-162.16410856405977	-10145.27733217148	31.245220695520096	329126.5532500726	328265.66037740244	2799.3763025646913	0.031183739970721044	-1.1120859949082385E-4	-0.9995136627350755	-0.9811818843788818	0.19064031021506495	-0.03063301956731138	-0.19055100138321474	-0.981659951181544	-0.005835761998476635
"

radius_landmarks_str_R324_Right_A <- "
-8.308990478515625e+001 -9.098584175109863e+000 -8.978558349609375e+001
-1.092769012451172e+002 -3.688332319259644e+000 -1.027557220458984e+002
-9.925588226318359e+001 -5.634788990020752e+000 -1.007320175170898e+002
-1.009435119628906e+002 -3.431178569793701e+000 -3.538116149902344e+002
"

res_radius <- orient_longbone(
  mode = "RADIUS",
  longitudinal_matrix_str = longitudinal_matrix_str_radius,
  dicom_iop = dicom_iop_str_radius,
  landmarks_str = radius_landmarks_str_R324_Right_A,
  section_loc = c(35, 50),
  individual_id = "R324_Right",
  camera_distance_mm = 300
)

res_radius$summary
#View(res_radius$summary)
res_radius$manual_orientation

cat(get_tcl(res_radius, section = "SECTION_35"))
cat(get_tcl(res_radius, section = "SECTION_50"))

copy_tcl(res_radius, section = "SECTION_35")
copy_tcl(res_radius, section = "SECTION_50")


# 4B. RADIUS: CT/DICOM true cross-section + 3D Slicer workflow ===============
#
# Same specimen and section locations as in the main RADIUS example above, but
# with the landmarks represented as if copied from the 3D Slicer Markups table.

radius_landmarks_slicer_R324_Right_B <- "
1  -83.089904785156 -9.098584175110  -89.785583496094 0 0 0 1 1 1 0 F-1 2 0
2 -109.276901245117 -3.688332319260 -102.755722045898 0 0 0 1 1 1 0 F-2 2 0
3  -99.255882263184 -5.634788990021 -100.732017517090 0 0 0 1 1 1 0 F-3 2 0
4 -100.943511962891 -3.431178569794 -353.811614990234 0 0 0 1 1 1 0 F-4 2 0
"

res_radius_true_slicer <- orient_longbone(
  mode = "RADIUS",
  longitudinal_matrix_str = longitudinal_matrix_str_radius,
  dicom_iop = dicom_iop_str_radius,
  landmarks_str = radius_landmarks_slicer_R324_Right_B,
  section_loc = c(35, 50),
  individual_id = "R324_Right",
  SOLID = FALSE,
  SLICER = TRUE,
  USE_ANAT_ORIENT = TRUE,
  volume_name = "AAM_T-324_radio_D",
  camera_distance_mm = 300,
  lm_coord_system = "LPS"
)

res_radius_true_slicer$summary
#View(res_radius_true_slicer$summary)

cat(get_slicer_py(res_radius_true_slicer, section = "SECTION_35"))
cat(get_slicer_py(res_radius_true_slicer, section = "SECTION_50"))

copy_slicer_py(res_radius_true_slicer, section = "SECTION_35")
copy_slicer_py(res_radius_true_slicer, section = "SECTION_50")


# 4C. RADIUS: solid mesh + 3D Slicer workflow ================================
#
# Replace mesh_file_radius with the full path to the corresponding closed
# surface mesh. The directory is illustrative; the file name is kept
# consistent with the radial example above.

mesh_file_radius <- "C:/Users/Alex/Desktop/AAM_T-324_radio_D.ply"

radius_landmarks_slicer_R324_Right_C <- "
1  -83.089904785156 -9.098584175110  -89.785583496094 0 0 0 1 1 1 0 F-1 2 0
2 -109.276901245117 -3.688332319260 -102.755722045898 0 0 0 1 1 1 0 F-2 2 0
3  -99.255882263184 -5.634788990021 -100.732017517090 0 0 0 1 1 1 0 F-3 2 0
4 -100.943511962891 -3.431178569794 -353.811614990234 0 0 0 1 1 1 0 F-4 2 0
"

if (file.exists(mesh_file_radius)) {
  res_radius_solid_slicer <- orient_longbone(
    mode = "RADIUS",
    mesh_file = mesh_file_radius,
    landmarks_str = radius_landmarks_slicer_R324_Right_C,
    lm_coord_system = "LPS",
    section_loc = c(35, 50),
    individual_id = "R324_Right",
    model_name = "AAM_T-324_radio_D",
    SOLID = TRUE,
    SLICER = TRUE,
    USE_ANAT_ORIENT = TRUE,
    camera_distance_mm = 300
  )
  
  res_radius_solid_slicer$summary
  #View(res_radius_solid_slicer$summary)
  res_radius_solid_slicer$mesh_axes$eigenvectors
  
  cat(get_slicer_py(res_radius_solid_slicer, section = "SECTION_35"))
  cat(get_slicer_py(res_radius_solid_slicer, section = "SECTION_50"))
  
  copy_slicer_py(res_radius_solid_slicer, section = "SECTION_35")
  copy_slicer_py(res_radius_solid_slicer, section = "SECTION_50")
} else {
  message("Skipping 4C: radial mesh file not found. Edit mesh_file_radius to run this example.")
}


# 5. HUMERUS_TABLE example ===================================================
#
# Landmark order for mode = "HUMERUS_TABLE":
#
#   LM1 = distal landmark for biomechanical length
#   LM2 = proximal landmark for biomechanical length
#
# This mode is intended for humeri scanned in a standardized table position,
# where the scan orientation carries anatomical information.

humerus_table_landmarks_str_H108_Right_A <- "
182.2418 -6.976971 -59.92139
182.7214  8.127365 -345.48276
"

res_humerus_table <- orient_longbone(
  mode = "HUMERUS_TABLE",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  dicom_iop = dicom_iop_str_humerus,
  landmarks_str = humerus_table_landmarks_str_H108_Right_A,
  section_loc = c(35, 50),
  individual_id = "H108_Right_Table",
  camera_distance_mm = 300
)

res_humerus_table$summary
#View(res_humerus_table$summary)
res_humerus_table$manual_orientation

cat(get_tcl(res_humerus_table, section = "SECTION_35"))
cat(get_tcl(res_humerus_table, section = "SECTION_50"))

copy_tcl(res_humerus_table, section = "SECTION_35")
copy_tcl(res_humerus_table, section = "SECTION_50")