# OrientCSG 0.3.2.9000

* Added DICOM Image Orientation (Patient) handling for BoneJ eigenvector matrices in `orient_longbone()`. Classic DICOM/BoneJ workflows now derive the BoneJ-to-internal coordinate transform from `dicom_iop` instead of always assuming the legacy `(-x, -y, z)` correction.
* Added advanced `bonej_coord_transform` options (`"dicom_iop"`, `"legacy_flip_xy"`, `"none"`, and `"manual"`) for reproducibility and diagnostics.
* Added a longitudinal-axis diagnostic comparing the transformed BoneJ axis with the anatomical distal-proximal reference defined by the landmarks.
* Updated long-bone examples and tests to include the DICOM Image Orientation (Patient) line.

# OrientCSG 0.3.1

* Updated mandibular 3D Slicer Python output so that in-plane slice orientation is controlled by the ARP normal projected into each section plane. This forces the ARP to appear horizontal in the captured slice and makes the Red slice view more closely match the anatomical orientation produced by the Amira/Avizo route.
* `X_SCREEN_REFERENCE` is now used only to resolve the left-right sign of the displayed section, not as the primary in-plane orientation vector.

# OrientCSG 0.3.0

* Added the mandibular 3D Slicer backend for CS1, CS2, and CS3 volume workflows.
