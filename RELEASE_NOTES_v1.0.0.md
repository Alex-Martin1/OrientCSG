# OrientCSG 1.0.0

This release expands the long-bone workflow beyond tibial and humeral sections by adding anatomical orientation support for the femur and radius.

## Added

- `mode = "FEMUR"` in `orient_longbone()`.
  - Landmark order: `Condyle1`, `Condyle2`, `SuperiorNeck`.
  - Biomechanical length is computed as the projected distance along the femoral longitudinal axis between `SuperiorNeck` and the midpoint of `Condyle1` and `Condyle2`.
- `mode = "RADIUS"` in `orient_longbone()`.
  - Landmark order: `RadialStyloid`, `UlnarNotch`, `DistArticular`, `ProxArticular`.
  - Biomechanical length is computed as the projected distance along the radial longitudinal axis between `DistArticular` and `ProxArticular`.
- Avizo/Amira TCL generation for femoral and radial sections.
- 3D Slicer Python generation for femoral and radial TRUE-volume workflows.
- 3D Slicer Python generation for femoral and radial solid-mesh workflows.
- Distal-to-proximal axis sign checks for femoral and radial modes.

## Updated

- README long-bone scope and landmark-order notes.
- `orient_longbone()` documentation and manual page.
- Long-bone examples.
- Unit tests covering femoral and radial mode creation and Slicer TRUE-volume output.

## Notes

`HUMERUS_TABLE` remains restricted to the Avizo/Amira table-position route when full anatomical orientation is requested. The Slicer backend is implemented for `TIBIA`, `HUMERUS`, `FEMUR`, and `RADIUS`.
