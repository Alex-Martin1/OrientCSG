devtools::load_all()

# ============================================================
# Synthetic BoneJ eigenvector matrix
# ============================================================
# The first column is interpreted as the longitudinal vector.
# After the BoneJ-to-Avizo correction, this gives L = c(0, 0, 1).

longitudinal_matrix_str_tibia <- "
||0.008|-0.758|-0.653||
||0.017|-0.652|0.758||
||1.000|0.017|-0.008||
"
longitudinal_matrix_str_humerus <- "
||0.023|0.973|0.231||
||0.022|-0.232|0.972||
||0.999|-0.018|-0.027||
" 

# ============================================================
# 1. TIBIA TEST
# ============================================================
# Landmark order:
# P1 = plateau landmark 1
# P2 = plateau landmark 2
# P3 = tibio-talar landmark

tibia_landmarks_str <- "
130.94606	-12.514749	-392.244507
164.351898	-17.573267	-395.017944
146.258621	-15.388991	-61.599937
"

res_tibia <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str_tibia,
  landmarks_str = tibia_landmarks_str,
  section_loc = c(50),
  individual_id = "TIBIA_TEST_T108",
  camera_distance_mm = 300
)

print(res_tibia)
View(res_tibia$summary)
View(res_tibia$manual_orientation)

stopifnot(inherits(res_tibia, "orientcsg_orientation"))
stopifnot(res_tibia$type == "TIBIA")
stopifnot(all(c("SECTION_50") %in% names(res_tibia$avizo_tcl)))
stopifnot(is.data.frame(res_tibia$summary))
stopifnot(is.data.frame(res_tibia$manual_orientation))

cat("\n\n================ TIBIA SECTION 50 TCL ================\n\n")
cat(res_tibia$avizo_tcl[["SECTION_50"]])


copy_tcl(res_tibia, section = "SECTION_50")

# ============================================================
# 2. HUMERUS TEST
# ============================================================
# Landmark order:
# P1 = medial trochlea anterior landmark
# P2 = capitulum anterior landmark
# P3 = distal biomechanical length landmark
# P4 = proximal biomechanical length landmark

humerus_landmarks_str <- "
1.647897491455078e+002 -1.567003917694092e+001 -6.820565032958984e+001 
1.863933868408203e+002 -1.576045989990234e+001 -6.810215759277344e+001
182.2418	-6.976971	-59.92139
182.7214	8.127365	-345.48276
"

res_humerus <- orient_longbone(
  mode = "HUMERUS",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  landmarks_str = humerus_landmarks_str,
  section_loc = c(35, 50),
  individual_id = "HUMERUS_TEST_T108",
  camera_distance_mm = 300
)

print(res_humerus)
View(res_humerus$summary)
View(res_humerus$manual_orientation)

stopifnot(inherits(res_humerus, "orientcsg_orientation"))
stopifnot(res_humerus$type == "HUMERUS")
stopifnot(all(c("SECTION_35", "SECTION_50") %in% names(res_humerus$avizo_tcl)))
stopifnot(is.data.frame(res_humerus$summary))
stopifnot(is.data.frame(res_humerus$manual_orientation))

cat("\n\n================ HUMERUS SECTION 35 TCL ================\n\n")
cat(res_humerus$avizo_tcl[["SECTION_35"]])

cat("\n\n================ HUMERUS SECTION 50 TCL ================\n\n")
cat(res_humerus$avizo_tcl[["SECTION_50"]])


copy_tcl(res_humerus, section = "SECTION_35")


copy_tcl(res_humerus, section = "SECTION_50")

# ============================================================
# 3. HUMERUS_TABLE TEST
# ============================================================
# Landmark order:
# P1 = distal landmark
# P2 = proximal landmark
#
# In this mode, ML is not defined from landmarks. It is defined as
# the projection of the scanner X axis onto the transverse plane.

humerus_table_landmarks_str <- "
182.2418	-6.976971	-59.92139
182.7214	8.127365	-345.48276
"

res_humerus_table <- orient_longbone(
  mode = "HUMERUS_TABLE",
  longitudinal_matrix_str = longitudinal_matrix_str_humerus,
  landmarks_str = humerus_table_landmarks_str,
  section_loc = c(35, 50),
  individual_id = "HUMERUS_TABLE_TEST_T108",
  camera_distance_mm = 300
)

print(res_humerus_table)
View(res_humerus_table$summary)
View(res_humerus_table$manual_orientation)

stopifnot(inherits(res_humerus_table, "orientcsg_orientation"))
stopifnot(res_humerus_table$type == "HUMERUS_TABLE")
stopifnot(all(c("SECTION_35", "SECTION_50") %in% names(res_humerus_table$avizo_tcl)))
stopifnot(is.data.frame(res_humerus_table$summary))
stopifnot(is.data.frame(res_humerus_table$manual_orientation))

cat("\n\n================ HUMERUS_TABLE SECTION 35 TCL ================\n\n")
cat(res_humerus_table$avizo_tcl[["SECTION_35"]])

cat("\n\n================ HUMERUS_TABLE SECTION 50 TCL ================\n\n")
cat(res_humerus_table$avizo_tcl[["SECTION_50"]])


copy_tcl(res_humerus_table, section = "SECTION_35")


copy_tcl(res_humerus_table, section = "SECTION_50")


