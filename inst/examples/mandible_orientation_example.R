# If OrientCSG is not installed yet, run:
#
# install.packages("remotes")
#remotes::install_github("Alex-Martin1/OrientCSG")

# Then load the package:

library(OrientCSG)



# This example shows the default mandibular workflow:
#
#   11 landmarks
#   complete_arch = FALSE
#   compute_bigonial = TRUE
#   estimate_lm10 = FALSE
#
# This corresponds to a mandibular specimen fragmented approximately by half, where one side is sufficiently preserved to place LM1-LM11 following the
# default landmark protocol. In this case, LM1_Line and the contralateral gonion are estimated geometrically.
#


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
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  compute_bigonial = TRUE,
  estimate_lm10 = FALSE
)

res
res$summary
res$measurements

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(get_tcl(res, section = "CS1"))

# To copy the command block to the clipboard, run:
copy_tcl(res, section = "CS1")
