# If OrientCSG is not installed yet, run:
#
#install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG")
#
# Then load the package:

library(OrientCSG)

# This script illustrates the three long-bone orientation modes currently supported by OrientCSG:

#   1. TIBIA
#   2. HUMERUS
#   3. HUMERUS_TABLE




# 1. Tibia example ====
#
# Landmark order for mode = "TIBIA":
#
#   LM1 = tibial plateau landmark 1
#   LM2 = tibial plateau landmark 2
#   LM3 = tibio-talar landmark

longitudinal_matrix_str_tibia <- "
||0.011|-0.780|-0.626||
||0.019|-0.626|0.780||
||1.000|0.021|-0.008||
"

tibia_landmarks_str <- "
130.94606  -12.514749 -392.244507
164.351898 -17.573267 -395.017944
146.258621 -15.388991  -61.599937
"

res_tibia <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str_tibia,
  landmarks_str = tibia_landmarks_str,
  section_loc = c(50),
  individual_id = "TIBIA_EXAMPLE",
  camera_distance_mm = 300
)

#res_tibia
res_tibia$summary
#res_tibia$manual_orientation

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(get_tcl(res_tibia, section = "SECTION_50"))

# To copy this command block to the clipboard, run:
copy_tcl(res_tibia, section = "SECTION_50")




# 2. Humerus example====
#
# Landmark order for mode = "HUMERUS":
#
#   P1 = distal landmark used to define the mediolateral reference direction
#   P2 = second distal landmark used to define the mediolateral reference direction
#   P3 = distal landmark for biomechanical length
#   P4 = proximal landmark for biomechanical length
#
# This mode should be used when the mediolateral anatomical direction is reconstructed from manually placed landmarks.

longitudinal_matrix_str_humerus <- "
||0.023|0.973|0.231||
||0.022|-0.232|0.972||
||0.999|-0.018|-0.027||
"

humerus_landmarks_str <- "
1.647897491455078e+002 -1.567003917694092e+001 -6.820565032958984e+001
1.863933868408203e+002 -1.576045989990234e+001 -6.810215759277344e+001
182.2418 -6.976971 -59.92139
182.7214  8.127365 -345.48276
"

res_humerus <- orient_longbone(
  mode = "HUMERUS",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  landmarks_str = humerus_landmarks_str,
  section_loc = c(35, 50),
  individual_id = "HUMERUS_EXAMPLE",
  camera_distance_mm = 300
)

res_humerus
res_humerus$summary
res_humerus$manual_orientation

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(get_tcl(res_humerus, section = "SECTION_35"))
cat(get_tcl(res_humerus, section = "SECTION_50"))


# To copy a command block to the clipboard, run:
copy_tcl(res_humerus, section = "SECTION_35")
copy_tcl(res_humerus, section = "SECTION_50")



# 3. Humerus table-position example====
#
# Landmark order for mode = "HUMERUS_TABLE":
#
#   P1 = distal landmark for biomechanical length
#   P2 = proximal landmark for biomechanical length
#
# This mode is intended for humeri scanned in a standardized table position, where the scan orientation carries anatomical information.

humerus_table_landmarks_str <- "
182.2418 -6.976971 -59.92139
182.7214  8.127365 -345.48276
"

res_humerus_table <- orient_longbone(
  mode = "HUMERUS_TABLE",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  landmarks_str = humerus_table_landmarks_str,
  section_loc = c(35, 50),
  individual_id = "HUMERUS_TABLE_EXAMPLE",
  camera_distance_mm = 300
)

res_humerus_table
res_humerus_table$summary
res_humerus_table$manual_orientation

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(get_tcl(res_humerus_table, section = "SECTION_35"))
cat(get_tcl(res_humerus_table, section = "SECTION_50"))


# To copy a command block to the clipboard, run:
copy_tcl(res_humerus_table, section = "SECTION_35")
copy_tcl(res_humerus_table, section = "SECTION_50")







# TIBIA: solid mesh + 3D Slicer workflow=====

# This workflow is intended for closed surface meshes (.ply, .stl, .obj).


# Replace mesh_file with the full path to your own mesh file. For example:
# mesh_file <- "C:/Users/Alex/Desktop/T108_solid.ply"

mesh_file <- "C:/Users/Alex/Desktop/T108_solid.ply"


# Landmarks exported/copied from 3D Slicer.
slicer_landmarks_str <- "
1 164.351898 -17.573267 -395.017944 0 0 0 1 1 1 0 F-1 2 0
2 130.946060 -12.514749 -392.244507 0 0 0 1 1 1 0 F-2 2 0
3 146.258621 -15.388991  -61.599937 0 0 0 1 1 1 0 F-3 2 0
"

res_solid_slicer <- orient_longbone(
  mode = "TIBIA",
  mesh_file = mesh_file,
  slicer_landmarks_str = slicer_landmarks_str,
  section_loc = 50,
  individual_id = "T108",
  model_name = "T108_solid",
  SOLID = TRUE,
  SLICER = TRUE
)

res_solid_slicer$summary

# Inspect the mesh-derived principal inertia axes
res_solid_slicer$mesh_axes$eigenvectors

cat(get_slicer_py(res_solid_slicer, "SECTION_50"))

copy_slicer_py(res_solid_slicer, "SECTION_50")
