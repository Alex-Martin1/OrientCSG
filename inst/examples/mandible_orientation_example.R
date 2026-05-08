# If OrientCSG is not installed yet, run:
#
# install.packages("remotes")
# remotes::install_github("Alex-Martin1/OrientCSG")

# Then load the package:

library(OrientCSG)



# This example shows the default mandibular workflow:
#
#   11 landmarks
#   complete_arch = FALSE
#   estimate_lm10 = FALSE
#   lm9_valid = TRUE
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
  estimate_lm10 = FALSE,
  lm9_valid = TRUE
)

res
res$summary
res$measurements

# Print the generated Avizo TCL block in a readable format for manual copying.
cat(get_tcl(res, section = "CS1"))
cat(get_tcl(res, section = "CS2"))
cat(get_tcl(res, section = "CS3"))

# To copy the command block to the clipboard, run:
copy_tcl(res, section = "CS1")

copy_tcl(res, section = "CS2")

copy_tcl(res, section = "CS3")


# MANDIBLE: 3D Slicer volume workflow =====

# If the same coordinates come from the Avizo/Amira-like workflow, leave
# lm_coord_system = "LPS". If the coordinates were copied directly from a
# 3D Slicer Markups table, use lm_coord_system = "RAS".
#
# Example of the same landmark set written as Slicer Markups-style rows:
landmarks_str_slicer_mandible <- "
1 30.802746 7.687321 -143.703278 0 0 0 1 1 1 0 F-1 2 0
2 1.575801 -7.369631 -105.562813 0 0 0 1 1 1 0 F-2 2 0
3 0.330960 22.722292 -93.421600 0 0 0 1 1 1 0 F-3 2 0
4 0.528437 16.996193 -108.474274 0 0 0 1 1 1 0 F-4 2 0
5 28.758656 5.048145 -132.697403 0 0 0 1 1 1 0 F-5 2 0
6 21.375843 4.792870 -134.530151 0 0 0 1 1 1 0 F-6 2 0
7 25.118534 0.853733 -121.618919 0 0 0 1 1 1 0 F-7 2 0
8 19.496758 0.576878 -123.945511 0 0 0 1 1 1 0 F-8 2 0
9 46.735912 35.260029 -164.050079 0 0 0 1 1 1 0 F-9 2 0
10 18.105160 -2.743076 -110.288727 0 0 0 1 1 1 0 F-10 2 0
11 47.784660 -12.426559 -201.179794 0 0 0 1 1 1 0 F-11 2 0
"

# The block below uses the LPS coordinates already defined in landmarks_str.
# To use the Slicer Markups-style table above instead, set:
# landmarks_str = landmarks_str_slicer_mandible
# lm_coord_system = "RAS"

res_slicer <- orient_mandible(
  landmarks_str = landmarks_str,
  individual_id = "MANDIBLE_EXAMPLE",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  estimate_lm10 = FALSE,
  lm9_valid = TRUE,
  lm_coord_system = "LPS",
  SLICER = TRUE,
  volume_name = "MANDIBLE_VOLUME"
)

res_slicer$summary
res_slicer$measurements

cat(get_slicer_py(res_slicer, section = "CS1"))
cat(get_slicer_py(res_slicer, section = "CS2"))
cat(get_slicer_py(res_slicer, section = "CS3"))

copy_slicer_py(res_slicer, section = "CS1")
copy_slicer_py(res_slicer, section = "CS2")
copy_slicer_py(res_slicer, section = "CS3")

# After pasting a mandibular Slicer block into the Python Interactor, two helper
# commands are available in Slicer:
#
# restore_view()
# refresh_orientcsg_scale()
