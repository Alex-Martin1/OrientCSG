library(OrientCSG)

# Example mandibular landmark coordinates.
# The coordinates must be provided in the fixed order LM1 to LM11.
landmarks_str <- "
-30.802746 -7.687321 -143.703278
 -1.575801  7.369631 -105.562813
 -0.330960 -22.722292  -93.421600
 -0.528437 -16.996193 -108.474274
-28.758656  -5.048145 -132.697403
-21.375843  -4.792870 -134.530151
-25.118534  -0.853733 -121.618919
-19.496758  -0.576878 -123.945511
-46.735912 -35.260029 -164.050079
-18.105160   2.743076 -110.288727
-47.784660  12.426559 -201.179794
"

res <- orient_mandible(
  landmarks_str = landmarks_str,
  individual_id = "MANDIBLE_EXAMPLE",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT"
)

res
res$summary
res$measurements

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(res$avizo_tcl$CS1)

# To copy the command block to the clipboard, run:
copy_tcl(res, section = "CS1")
