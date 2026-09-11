# OrientCSG 1.0.1

OrientCSG 1.0.1 is a stable maintenance release correcting the orientation of BoneJ-derived longitudinal axes in TRUE-volume DICOM workflows.

## Main change

`orient_longbone()` now uses DICOM Image Orientation (Patient) together with Image Position (Patient) from two consecutive slices in the exact order used by the ImageJ/BoneJ stack. IOP defines the two in-plane axes; the ordered IPP pair determines the sign of the stack Z axis. This removes the ambiguity that occurs when DICOM slice order runs opposite to the normal obtained from the IOP cross product.

The correction is applied before long-bone geometry is constructed, so it propagates to all `SOLID = FALSE` modes and both TRUE-volume output backends (Avizo/Amira TCL and 3D Slicer Python).

## API and output

TRUE-volume calls now require `dicom_iop`, `dicom_ipp_1`, and `dicom_ipp_2`. The IPP values may come from any two consecutive slices, provided `dicom_ipp_1` precedes `dicom_ipp_2` in the BoneJ stack.

The long-bone summary column `Bio_length` is renamed `Bio_Length_&_Orient`. For TRUE-volume anatomical workflows it records biomechanical length followed by the IOP, IPP1, and IPP2 values used to construct the coordinate transformation. The numeric value remains available as `res$biomechanical_length`, and the DICOM metadata remain available in structured form in `res$bonej`.

## Acquisition convention

The established standardized dry-bone scanning position remains required for consistent anterior/posterior display. DICOM geometry resolves scanner axes and slice order, but does not infer anatomical anterior/posterior if a specimen is physically rotated around its long axis.

## Validation

Tests cover both possible slice-order signs, malformed/inconsistent IPP input, and the T109 geometry that identified the IOP-only ambiguity. Existing tibial, humeral, femoral, radial, Avizo/Amira, Slicer, and SOLID workflows remain covered.
