# OrientCSG 1.0.2

* Corrected TRUE-volume tibial and femoral in-plane orientation so the two non-directional transverse landmarks define an undirected axis: swapping LM1 and LM2 no longer changes the final ML/AP vectors or the displayed anterior-up orientation.
* Removed the historical tibial Slicer-table row swap from landmark parsing; identical coordinates now produce identical geometry whether supplied as plain XYZ rows or copied as a Slicer-style table.
* Retained the established CT acquisition convention used to resolve anterior/posterior sign for tibiae and femora. The TRUE-volume and solid-mesh Slicer backends now share the same anatomical screen convention and preserve a proximal viewing side.

# OrientCSG 1.0.1

* Explicitly documented and regression-tested support for current BoneJ Moments of Inertia Log output pasted verbatim as three `[INFO] ||...||` eigenvector rows; no manual removal of console prefixes or pipe characters is required.
* Corrected TRUE-volume BoneJ axis conversion by using DICOM Image Orientation (Patient) together with Image Position (Patient) from two consecutive slices in BoneJ stack order. The ordered IPP pair determines whether stack Z follows or opposes the IOP-derived normal.
* Consolidated TRUE-volume DICOM metadata into the single `dicom_orientation` argument. IOP and the two consecutive IPP lines can still be defined separately in the calling script and combined with `c(dicom_iop, dicom_ipp_1, dicom_ipp_2)`; the ordered IPP pair continues to determine the stack-Z sign.
* Updated long-bone result summaries: the former `Bio_length` column is now `Bio_Length_&_Orient`, containing biomechanical length followed by the IOP, IPP1, and IPP2 values used for TRUE-volume orientation. The numeric length is also retained as `res$biomechanical_length`.
* Documented that anterior/posterior display in TRUE-volume long-bone workflows assumes standardized anatomical positioning of the dry specimen during CT acquisition.
* Added regression coverage for opposite slice-order signs, including the T109 geometry that exposed the IOP-only ambiguity, and updated examples and documentation for the new DICOM inputs.


* Corrected mandibular CS1/CS2 viewing-side selection so Avizo/Amira and 3D Slicer use the anatomical anterior reference (`LM0 -> LM2`) rather than the LM1-side reference. CS3 retains the existing `lm1_side` convention.
* Corrected the in-plane femoral capture orientation so that the anterior aspect is displayed at the top in Avizo/Amira TCL output, TRUE-volume 3D Slicer output, and solid-mesh 3D Slicer output.
* Added direct three-component BoneJ longitudinal-vector input to `longitudinal_matrix_str`, alongside the existing legacy 3 x 3 matrix and full Results-table row formats.
* Added regression tests and updated the installed long-bone examples and function documentation for both changes.

# OrientCSG 1.0.0

* Added long-bone anatomical orientation support for `FEMUR` and `RADIUS` modes in `orient_longbone()`.
* Implemented femoral biomechanical length as the projected distance along the femoral longitudinal axis between the distal condylar midpoint (`Condyle1`/`Condyle2`) and `SuperiorNeck`.
* Implemented radial biomechanical length as the projected distance along the radial longitudinal axis between `DistArticular` and `ProxArticular`.
* Extended distal-to-proximal longitudinal-axis sign checks, Avizo/Amira TCL output, TRUE-volume 3D Slicer output, and solid-mesh 3D Slicer output to femora and radii.
* Updated README, examples, manual pages, and tests for the new femoral and radial landmark definitions.

# OrientCSG 0.3.4

* Updated long-bone 3D Slicer Python output so the public restoration command is consistently `restore_view()` in both TRUE-volume and solid-mesh routes. TRUE-volume blocks now use `restore_3d_camera()` for camera-only restoration, and obsolete `restore_orientcsg_camera_state()` instructions were removed.
* Added support for BoneJ Results-table row input in `longitudinal_matrix_str`; the legacy 3 x 3 eigenvector matrix is still accepted.
* Added `USE_ANAT_ORIENT` to `orient_longbone()`.
* When `USE_ANAT_ORIENT = TRUE`, the function preserves the previous anatomical-orientation workflow.
* When `USE_ANAT_ORIENT = FALSE`, the function accepts a single landmark as the section point, places the section perpendicular to the longitudinal axis, and omits the anatomical AP and ML reference planes.
* Implemented this section-only mode for `TIBIA`, `HUMERUS`, and `HUMERUS_TABLE`.

# OrientCSG 0.3.3

* Removed deprecated public aliases `slicer_landmarks_str` and `landmark_coordinate_system`; use `landmarks_str` and `lm_coord_system` instead.
* Clarified coordinate-system handling for 3D Slicer workflows. `lm_coord_system` now explicitly refers to the numeric values that reach R: coordinates copied or exported from Slicer Markups may paste/write as LPS even when the interface displays R/A/S columns, whereas values extracted explicitly with `GetNthControlPointPositionWorld()` should be treated as true Slicer world RAS.
* Updated README, examples, function documentation, and manual pages to reflect this distinction for mandibular and long-bone workflows.
* Forced the tibial longitudinal vector in `orient_longbone()` to point from the tibio-talar landmark toward the midpoint of the two plateau landmarks. This stabilizes proximal-view Slicer output for tibial solid-mesh workflows.
* Updated long-bone Slicer camera construction so the viewing side is selected from the anatomical distal-to-proximal vector rather than from a global Slicer axis.
* Corrected tibial Slicer view orientation so anterior is placed upward in the generated view.
* Oriented the mandibular ARP normal (`Vec_Penp`) anatomically from inferior toward superior using the priority real LM9, then LM3/LM4, then orientation-only LM9. The same signed vector is used by both Avizo/Amira TCL and 3D Slicer Python outputs.
* Replaced the older `cs3_camera_side` concept with `lm1_side`, which declares whether LM1 was placed on the right or left mandibular side. This side is now used to select the viewing side for CS1/CS2 and the anatomical side convention for CS3 in both Avizo/Amira and 3D Slicer outputs.

* Added a longitudinal-axis diagnostic comparing the transformed BoneJ axis with the anatomical distal-proximal reference defined by the landmarks.

# OrientCSG 0.3.1

* Updated mandibular 3D Slicer Python output so that in-plane slice orientation is controlled by the ARP normal projected into each section plane. This forces the ARP to appear horizontal in the captured slice and makes the Red slice view more closely match the anatomical orientation produced by the Amira/Avizo route.
* `X_SCREEN_REFERENCE` is now used only to resolve the left-right sign of the displayed section, not as the primary in-plane orientation vector.

# OrientCSG 0.3.0

* Added the mandibular 3D Slicer backend for CS1, CS2, and CS3 volume workflows.
