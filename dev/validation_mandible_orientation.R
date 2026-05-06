install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG", upgrade = "never", force = TRUE)
library(OrientCSG)
packageVersion("OrientCSG")
devtools::load_all()


# Helper for visual inspection

inspect_mandible_result <- function(res) {
  View(res$summary)
  View(res$measurements)
  
  cat("\n\n# ============================================================\n")
  cat("# CS1 TCL\n")
  cat("# ============================================================\n\n")
  cat(res$avizo_tcl$CS1)
  
  cat("\n\n# ============================================================\n")
  cat("# CS2 TCL\n")
  cat("# ============================================================\n\n")
  cat(res$avizo_tcl$CS2)
  
  cat("\n\n# ============================================================\n")
  cat("# CS3 TCL\n")
  cat("# ============================================================\n\n")
  cat(res$avizo_tcl$CS3)
}


# Notes:
# - complete_arch = FALSE: LM4 keeps its default protocol meaning.
# - complete_arch = TRUE: LM4 is replaced by LM1_Line_Real / A_Line.
# - 12-landmark cases add LM12 as the contralateral gonion.
# - 9-landmark cases use LM1-LM9 only; LM10 and LM11 are absent.

# Base coordinates

# Default LM4 for complete_arch = FALSE
LM4_default <- "-5.284366607666016e-001 -1.699619293212891e+001 -1.084742736816406e+002"

# Real A_Line / LM1_Line for complete_arch = TRUE
LM1_Line_Real <- "3.010813331604004e+001 -5.403686523437500e+000 -1.453549194335938e+002"

# Contralateral gonion for 12-landmark cases
LM12_gonion_contralateral <- "4.843153762817383e+001 -3.108958625793457e+001 -1.679723052978516e+002"


# 11 landmarks for complete_arch = FALSE
landmarks_11_CAF <- "
-3.080274581909180e+001 -7.687320709228516e+000 -1.437032775878906e+002 
-1.575801372528076e+000 7.369630813598633e+000 -1.055628128051758e+002 
-3.309603333473206e-001 -2.272229194641113e+001 -9.342160034179688e+001 
-5.284366607666016e-001 -1.699619293212891e+001 -1.084742736816406e+002 
-2.875865554809570e+001 -5.048145294189453e+000 -1.326974029541016e+002 
-2.137584304809570e+001 -4.792869567871094e+000 -1.345301513671875e+002 
-2.511853408813477e+001 -8.537330627441406e-001 -1.216189193725586e+002 
-1.949675750732422e+001 -5.768775939941406e-001 -1.239455108642578e+002 
-4.673591232299805e+001 -3.526002883911133e+001 -1.640500793457031e+002 
-1.810515975952148e+001 2.743076324462891e+000 -1.102887268066406e+002 
-4.778466033935547e+001 1.242655944824219e+001 -2.011797943115234e+002
"


# 11 landmarks for complete_arch = TRUE
# Same sequence, but LM4 is replaced by LM1_Line_Real / A_Line.
landmarks_11_CAT <- "
-3.080274581909180e+001 -7.687320709228516e+000 -1.437032775878906e+002 
-1.575801372528076e+000 7.369630813598633e+000 -1.055628128051758e+002 
-3.309603333473206e-001 -2.272229194641113e+001 -9.342160034179688e+001 
3.010813331604004e+001 -5.403686523437500e+000 -1.453549194335938e+002 
-2.875865554809570e+001 -5.048145294189453e+000 -1.326974029541016e+002 
-2.137584304809570e+001 -4.792869567871094e+000 -1.345301513671875e+002 
-2.511853408813477e+001 -8.537330627441406e-001 -1.216189193725586e+002 
-1.949675750732422e+001 -5.768775939941406e-001 -1.239455108642578e+002 
-4.673591232299805e+001 -3.526002883911133e+001 -1.640500793457031e+002 
-1.810515975952148e+001 2.743076324462891e+000 -1.102887268066406e+002 
-4.778466033935547e+001 1.242655944824219e+001 -2.011797943115234e+002
"


# 12 landmarks for complete_arch = FALSE
landmarks_12_CAF <- paste(
  landmarks_11_CAF,
  LM12_gonion_contralateral,
  sep = "\n"
)


# 12 landmarks for complete_arch = TRUE
landmarks_12_CAT <- paste(
  landmarks_11_CAT,
  LM12_gonion_contralateral,
  sep = "\n"
)


# 9 landmarks for complete_arch = FALSE
# LM10 and LM11 are omitted.
landmarks_9_CAF <- "
-3.080274581909180e+001 -7.687320709228516e+000 -1.437032775878906e+002 
-1.575801372528076e+000 7.369630813598633e+000 -1.055628128051758e+002 
-3.309603333473206e-001 -2.272229194641113e+001 -9.342160034179688e+001 
-5.284366607666016e-001 -1.699619293212891e+001 -1.084742736816406e+002 
-2.875865554809570e+001 -5.048145294189453e+000 -1.326974029541016e+002 
-2.137584304809570e+001 -4.792869567871094e+000 -1.345301513671875e+002 
-2.511853408813477e+001 -8.537330627441406e-001 -1.216189193725586e+002 
-1.949675750732422e+001 -5.768775939941406e-001 -1.239455108642578e+002 
-4.673591232299805e+001 -3.526002883911133e+001 -1.640500793457031e+002
"


# ============================================================
# 1. Default:
# 11 landmarks + complete_arch = FALSE + lm9_valid = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_11_CAF,
  individual_id = "MANDIBLE_11_CAF_CBT",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 2. Both gonion preserved:
# 12 landmarks + complete_arch = FALSE + lm9_valid = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAF,
  individual_id = "MANDIBLE_12_CAF_CBT",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 3. Condyles absent:
# 9 landmarks + complete_arch = FALSE + lm9_valid = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_9_CAF,
  individual_id = "MANDIBLE_9_CAF_CBT",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 4. Condyles absent and invalid LM9 placeholder:
# 9 landmarks + complete_arch = FALSE + lm9_valid = FALSE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_9_CAF,
  individual_id = "MANDIBLE_9_CAF_CBF",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  lm9_valid = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 5. Complete arch, one gonion side only:
# 11 landmarks + complete_arch = TRUE + lm9_valid = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_11_CAT,
  individual_id = "MANDIBLE_11_CAT_CBT",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 6. Complete arch and both gonion preserved:
# 12 landmarks + complete_arch = TRUE + lm9_valid = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAT,
  individual_id = "MANDIBLE_12_CAT_CBT",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 7a. Estimated LM10:
# 11 landmarks + complete_arch = FALSE + estimate_lm10 = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_11_CAF,
  individual_id = "MANDIBLE_11_CAF_ELM10",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  estimate_lm10 = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 7b. Estimated LM10 with complete arch:
# 11 landmarks + complete_arch = TRUE + estimate_lm10 = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_11_CAT,
  individual_id = "MANDIBLE_11_CAT_ELM10",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = TRUE,
  estimate_lm10 = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 7c. Estimated LM10 with 12 landmarks:
# 12 landmarks + complete_arch = FALSE + estimate_lm10 = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAF,
  individual_id = "MANDIBLE_12_CAF_ELM10",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  estimate_lm10 = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 7d. Estimated LM10 with 12 landmarks and complete arch:
# 12 landmarks + complete_arch = TRUE + estimate_lm10 = TRUE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAT,
  individual_id = "MANDIBLE_12_CAT_ELM10",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = TRUE,
  estimate_lm10 = TRUE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 8. Both gonion preserved but LM9 treated as invalid placeholder:
# 12 landmarks + complete_arch = FALSE + lm9_valid = FALSE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAF,
  individual_id = "MANDIBLE_12_CAF_CBF",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = FALSE,
  lm9_valid = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")


# ============================================================
# 9. Complete arch and both gonion preserved, but LM9 treated as invalid placeholder:
# 12 landmarks + complete_arch = TRUE + lm9_valid = FALSE
# ============================================================

res <- orient_mandible(
  landmarks_str = landmarks_12_CAT,
  individual_id = "MANDIBLE_12_CAT_CBF",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT",
  complete_arch = TRUE,
  lm9_valid = FALSE
)

inspect_mandible_result(res)

copy_tcl(res, section = "CS1")
copy_tcl(res, section = "CS2")
copy_tcl(res, section = "CS3")