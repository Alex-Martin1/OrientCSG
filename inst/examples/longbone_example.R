library(OrientCSG)

longitudinal_matrix_str <- ""
landmarks_str <- ""

res <- orient_longbone(
  mode = "HUMERUS_TABLE",
  longitudinal_matrix_str = longitudinal_matrix_str,
  landmarks_str = landmarks_str,
  section_loc = c(35, 50),
  individual_id = "HUMERUS_001",
  camera_distance_mm = 300
)

View(res$summary)
cat(res$avizo_tcl[["SECTION_35"]])
