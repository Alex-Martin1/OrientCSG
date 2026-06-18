# OrientCSG 0.3.4

* Added support for BoneJ Results-table row input in `longitudinal_matrix_str`; the legacy 3 x 3 eigenvector matrix is still accepted.
* Added `USE_ANAT_ORIENT` to `orient_longbone()`.
* When `USE_ANAT_ORIENT = TRUE`, the function preserves the previous anatomical-orientation workflow.
* When `USE_ANAT_ORIENT = FALSE`, the function accepts a single landmark as the section point, places the section perpendicular to the longitudinal axis, and omits the anatomical AP and ML reference planes.
* Implemented this section-only mode for `TIBIA`, `HUMERUS`, and `HUMERUS_TABLE`.

# OrientCSG 0.3.3

* Removed deprecated public aliases `slicer_landmarks_str` and `landmark_coordinate_system`; use `landmarks_str` and `lm_coord_system` instead.
* Renamed the diagnostic BoneJ transform option `bonej_coord_transform = "legacy_flip_xy"` to `bonej_coord_transform = "flip_xy"`; the old spelling is no longer accepted.

* Clarified coordinate-system handling for 3D Slicer workflows. `lm_coord_system` now explicitly refers to the numeric values that reach R: coordinates copied or exported from Slicer Markups may paste/write as LPS even when the interface displays R/A/S columns, whereas values extracted explicitly with `GetNthControlPointPositionWorld()` should be treated as true Slicer world RAS.
* Updated README, examples, function documentation, and manual pages to reflect this distinction for mandibular and long-bone workflows.
* Forced the tibial longitudinal vector in `orient_longbone()` to point from the tibio-talar landmark toward the midpoint of the two plateau landmarks. This stabilizes proximal-view Slicer output for tibial solid-mesh workflows.
* Updated long-bone Slicer camera construction so the viewing side is selected from the anatomical distal-to-proximal vector rather than from a global Slicer axis.
* Corrected tibial Slicer view orientation so anterior is placed upward in the generated view.
* Oriented the mandibular ARP normal (`Vec_Penp`) anatomically from inferior toward superior using the priority real LM9, then LM3/LM4, then orientation-only LM9. The same signed vector is used by both Avizo/Amira TCL and 3D Slicer Python outputs.
* Replaced the older `cs3_camera_side` concept with `lm1_side`, which declares whether LM1 was placed on the right or left mandibular side. This side is now used to select the viewing side for CS1/CS2 and the anatomical side convention for CS3 in both Avizo/Amira and 3D Slicer outputs.

* Added DICOM Image Orientation (Patient) handling for BoneJ eigenvector matrices in `orient_longbone()`. Classic DICOM/BoneJ workflows now derive the BoneJ-to-internal coordinate transform from `dicom_iop` instead of always assuming the legacy `(-x, -y, z)` correction.
* Added advanced `bonej_coord_transform` options (`"dicom_iop"`, `"legacy_flip_xy"`, `"none"`, and `"manual"`) for reproducibility and diagnostics.
* Added a longitudinal-axis diagnostic comparing the transformed BoneJ axis with the anatomical distal-proximal reference defined by the landmarks.
* Updated long-bone examples and tests to include the DICOM Image Orientation (Patient) line.

# OrientCSG 0.3.1

* Updated mandibular 3D Slicer Python output so that in-plane slice orientation is controlled by the ARP normal projected into each section plane. This forces the ARP to appear horizontal in the captured slice and makes the Red slice view more closely match the anatomical orientation produced by the Amira/Avizo route.
* `X_SCREEN_REFERENCE` is now used only to resolve the left-right sign of the displayed section, not as the primary in-plane orientation vector.

# OrientCSG 0.3.0

* Added the mandibular 3D Slicer backend for CS1, CS2, and CS3 volume workflows.
