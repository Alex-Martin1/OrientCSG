library(OrientCSG)

# Paste 11 landmarks x 3 coordinates here, in order LM1...LM11.
landmarks_str <- ""

res <- orient_mandible(
  landmarks_str = landmarks_str,
  individual_id = "MANDIBLE_001",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT"
)

View(res$summary)
View(res$measurements)
cat(res$avizo_tcl$CS1)
copy_tcl(res, section = "CS1")
