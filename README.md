# OrientCSG

OrientCSG is an R package for reproducible orientation of mandibular and long-bone cross-sections in cross-sectional geometry workflows.

The package was designed to generate consistent anatomical reference systems for virtual section capture. It supports three broad types of workflows:

1. classic CT-derived workflows using BoneJ-derived principal axes, with Amira/Avizo TCL or 3D Slicer Python output;
2. solid surface mesh workflows using `.ply`, `.stl`, or `.obj` files, with optional 3D Slicer Python output; and
3. mandibular volume workflows with either Avizo/Amira TCL or 3D Slicer Python output.

OrientCSG computes section locations, anatomical vectors, camera/view parameters, summary tables, manual-orientation tables, Amira/Avizo TCL command blocks, and, where requested, 3D Slicer Python blocks.

The anatomical logic for long bones follows criteria grounded in Ruff’s (2002) proposals for long-bone orientation. The mandibular workflow is broadly comparable to the mandibular orientation approach of Toro-Ibacache et al. (2019).

## Installation

Install the current stable version from GitHub:

```r
install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG")
library(OrientCSG)
```

The solid mesh workflow uses `Rvcg` when `SOLID = TRUE`. Automatic DICOM metadata extraction through `dicom_dir` uses `oro.dicom`. Install either suggested package when you need that workflow:

```r
install.packages("Rvcg")
install.packages("oro.dicom")
```

## What OrientCSG does

OrientCSG takes anatomical landmark coordinates and computes reproducible reference systems for cross-section orientation.

Depending on the workflow, it can:

- compute anatomical points and vectors;
- compute section locations;
- compute long-bone longitudinal axes from a direct BoneJ longitudinal vector, current BoneJ Log output copied verbatim, a legacy 3 x 3 eigenvector matrix, a full BoneJ Results-table row, or a closed surface mesh;
- return summary tables and manual-orientation tables; TRUE-volume long-bone summaries store biomechanical length followed by the IOP/IPP values used for orientation in `Bio_Length_&_Orient`;
- generate Amira/Avizo TCL command blocks;
- generate 3D Slicer Python blocks for supported Slicer workflows;
- generate batch capture blocks with `flash_capture()` for Avizo/Amira + CT, 3D Slicer + CT, and 3D Slicer + SOLID mesh workflows;
- copy generated command blocks to the clipboard.

## What OrientCSG does not do

OrientCSG does not directly control Amira/Avizo or 3D Slicer from R. It generates command blocks that the user can paste into the relevant software.

OrientCSG does not segment CT data, choose CT thresholds, extract contours, or calculate cross-sectional geometry properties such as cortical area, total area, second moments of area, polar moment of area, or section modulus.

For DICOM or other volumetric image workflows, segmentation and thresholding must be handled before using OrientCSG. This is intentional: in volumetric data, the calculated axis depends on which voxels are treated as bone, so thresholding should remain explicit and user-controlled.

## Current scope

OrientCSG currently supports two main anatomical workflows.

### Mandibular cross-sections

The mandibular workflow is implemented in `orient_mandible()`.

It currently:

- defines the alveolar reference plane (ARP) from `LM1`, `LM2`, and `LM1_Line`;
- computes `CS1`, `CS2`, and `CS3` following the mandibular landmark protocol;
- supports 9-, 11-, and 12-landmark inputs to accommodate different preservation states;
- returns summary tables, mandibular size-related measurements with status/method metadata, manual-orientation tables, and one software command block per section. By default these are Amira/Avizo TCL blocks; with `SLICER = TRUE`, they are 3D Slicer Python blocks that orient the selected slice view to CS1, CS2, or CS3, activate a CT volume-rendering preset, create ARP and `LM1_Line` verification objects, add a 10 mm scale bar, and configure a 3D verification view.

### Long-bone cross-sections

The long-bone workflow is implemented in `orient_longbone()`.

It currently supports:

- `TIBIA`;
- `HUMERUS`;
- `FEMUR`;
- `RADIUS`;
- `ULNA`;
- `HUMERUS_TABLE`.

For classic CT-derived workflows, `orient_longbone()` accepts the BoneJ longitudinal direction as three direct vector components, current BoneJ Log output copied directly from the Log window (three `[INFO] ||...||` rows), the legacy compact 3 x 3 Moments of Inertia eigenvector matrix, or a full BoneJ Results-table row. Direct vector input is treated as the first BoneJ vector; for Log, matrix, or table input, the first vector is used as the longitudinal axis. The BoneJ vector or matrix is transformed from the ImageJ stack basis to the internal DICOM/LPS basis using three DICOM metadata lines from the exact stack used in BoneJ: Image Orientation (Patient) (`0020,0037`) plus Image Position (Patient) (`0020,0032`) from two consecutive slices supplied in stack order. IOP defines the in-plane axes and the ordered IPP pair resolves the actual direction of stack Z, including series whose slice order runs opposite to the IOP-derived normal.


Current BoneJ Log output can be pasted without editing, for example:

```r
longitudinal_matrix_str <- "
[INFO] ||0.018|-0.826|-0.563||
[INFO] ||-0.019|0.562|-0.827||
[INFO] ||-1.000|-0.026|0.005||
"
```

The `[INFO]` prefixes and pipe characters are ignored by the parser; the three rows are interpreted as the 3 x 3 eigenvector matrix exactly as printed by BoneJ.

For TRUE-volume anatomical orientation, the specimen must also follow the established standardized scanning-position convention. IOP/IPP resolves scanner geometry and slice order, but it cannot identify an anatomical anterior/posterior reversal caused by physically rotating a dry bone 180 degrees around its longitudinal axis. This convention avoids requiring additional anatomical landmarks. In `TIBIA` and `FEMUR`, the two plateau/condylar landmarks define an undirected transverse axis: swapping those two points does not change the TRUE-volume orientation; the acquisition convention resolves the final AP sign. In `ULNA`, AP is resolved anatomically: LM2 to LM3 is treated as posterior-to-anterior. Because LM3 should be anterior to both trochlear-waist landmarks, swapping LM1 and LM2 does not change the final ML/AP axes.

#### ULNA landmarks

`ULNA` uses four landmarks:

| Order | Name | Location |
|---|---|---|
| LM1 | `TrochlearWaistLat` | Lateral point of the narrowest waist of the trochlear notch, placed on the trochlear articular edge. |
| LM2 | `TrochlearWaistMed` | Medial point of the narrowest waist of the trochlear notch, placed on the trochlear articular edge. If the articular edge disappears at the waist, place the landmark at the same anteroposterior depth indicated by the adjacent trochlear articular edge immediately proximal to the waist. |
| LM3 | `RadialTrochlearBorder` | Point on the border between the radial and trochlear articular surfaces, placed as far as possible toward the centre of the overall olecranon articular surface while remaining on the border. |
| LM4 | `UlnarHeadDistal` | Most distal point of the articular portion of the ulnar head, excluding the styloid process. |

LM1 and LM2 are interchangeable. Their connecting line defines ML. LM2 to LM3 is used only to resolve the posterior-to-anterior sign of AP. Ulnar biomechanical length is the projected distance along the longitudinal axis from distal LM4 to proximal LM3.

For closed surface meshes, `orient_longbone()` can compute the longitudinal axis directly from the mesh when `SOLID = TRUE`. The mesh is treated as a homogeneous closed solid, and the eigenvector associated with the smallest principal moment of inertia is used as the longitudinal axis.

When `SLICER = TRUE`, OrientCSG generates a 3D Slicer Python block that creates the oriented section and sets the 3D view. This Slicer workflow is currently implemented for:

- `TIBIA`;
- `HUMERUS`;
- `FEMUR`;
- `RADIUS`;
- `ULNA`.

It is intentionally not implemented for `HUMERUS_TABLE`, because that mode relies on a standardized scanner/table orientation rather than the landmark-based anatomical workflow used by the supported Slicer modes.

### Viewing distance / zoom

`camera_distance` is a relative visual-framing factor, not a physical camera distance in millimetres. The default `camera_distance = 1` uses the calibrated standard view. Values below `1` zoom in and values above `1` zoom out. For long-bone workflows, OrientCSG maps the same factor to the native orthographic zoom control of each backend: Avizo/Amira uses a base `CameraHeight` of 100, Slicer TRUE uses a base Red-slice vertical field of view of 70 mm, and Slicer 3D views (including SOLID sections and TRUE verification views) use a base `ParallelScale` of 35, corresponding to approximately 70 mm of visible vertical height. The Slicer TRUE horizontal field of view is derived from the current Red-view aspect ratio so physical X/Y scale remains consistent. Mandibular Slicer output retains its established 85 mm base vertical field of view (`ParallelScale = 42.5` in 3D), with the same relative `camera_distance` scaling.

## Coordinate conventions

The classic Amira/Avizo workflow generally uses the external mesh/Avizo coordinate convention used in the input data.

3D Slicer works internally in RAS coordinates, but Markups coordinates that are copied from the table or exported to common Markups files may paste/write as LPS. This can happen even when the Slicer table displays R/A/S column labels. In OrientCSG, `lm_coord_system` must describe the numeric values that actually arrive in R, not the coordinate labels visible in the Slicer interface.

Practical rule:

- coordinates copied manually from a Slicer Markups table, or exported from Slicer Markups without verifying the header, should usually be treated as `lm_coord_system = "LPS"`;
- coordinates extracted explicitly as Slicer world coordinates with Python, for example with `GetNthControlPointPositionWorld()`, should be treated as `lm_coord_system = "RAS"`.

The text format and the coordinate system are handled separately. Slicer-style rows are parsed as tables, but the spatial interpretation still comes only from `lm_coord_system`. RAS and LPS differ by the sign of X and Y:

```text
x_RAS = -x_LPS
y_RAS = -y_LPS
z_RAS =  z_LPS
```

If true Slicer world RAS coordinates are required, extract them from the Slicer Python Interactor instead of using manual table copy. For example:

```python
markupsNode = slicer.util.getNode("NAME_OF_MARKUPS_NODE")
for i in range(markupsNode.GetNumberOfControlPoints()):
    p = [0.0, 0.0, 0.0]
    markupsNode.GetNthControlPointPositionWorld(i, p)
    label = markupsNode.GetNthControlPointLabel(i)
    print(i + 1, label, p[0], p[1], p[2])
```

For classic BoneJ workflows, the landmark coordinate system and the BoneJ stack transformation are separate issues. `lm_coord_system` only controls the landmarks. For routine TRUE-volume analyses, the recommended workflow is now to supply `dicom_dir`, pointing to the local DICOM directory used to build the ImageJ/BoneJ stack. OrientCSG reads only enough files to obtain two readable DICOM headers, never loads pixel data, verifies that the selected `InstanceNumber` values are consecutive, orders the pair by `InstanceNumber`, and derives Image Orientation (Patient) plus the ordered Image Position (Patient) pair automatically. The full CT series is not read. Manual `dicom_orientation = c(IOP, IPP1, IPP2)` remains supported for legacy or validation workflows. The IOP/IPP values actually used are reported in `res$summary`, while additional transformation diagnostics remain available in `res$bonej`.

## Preservation requirements

For long bones, OrientCSG currently assumes complete or near-complete specimens. Biomechanical length, section locations, and anatomical axes require preservation of the relevant proximal and distal anatomy.

For mandibles, the workflow can be applied to fragmented specimens if the anatomical region required by the landmark protocol is sufficiently preserved. The function accepts 9, 11, or 12 landmarks. The 9-landmark input is intended for cases where `LM10` and `LM11` cannot be placed. The 12-landmark input adds `LM12` as the contralateral gonion, allowing direct computation of bigonial breadth.

## Examples

The README intentionally provides only a compact overview of package use.
Complete executable examples are distributed with the package in `inst/examples/`.
These installed examples should be treated as the main practical reference because they show the full input structure for each supported workflow.
The long-bone script demonstrates all four accepted BoneJ text forms: current Log output copied verbatim, a direct three-component vector, the legacy compact 3 x 3 matrix, and a full Results-table row.

The long-bone example script includes:

- `TIBIA`;
- `HUMERUS`;
- `FEMUR`;
- `RADIUS`;
- `ULNA`;
- `HUMERUS_TABLE`;
- CT/DICOM true cross-section + 3D Slicer workflows;
- solid mesh + 3D Slicer workflows;
- a tibial `flash_capture()` batch example using 20, 35, 50, 65, and 80% sections.

The mandibular example script shows the mandibular orientation workflow and the corresponding Amira/Avizo and 3D Slicer outputs.

After installation, the example directory can be located with:

```r
system.file("examples", package = "OrientCSG")
list.files(system.file("examples", package = "OrientCSG"))
```

To inspect the long-bone example script:

```r
example_file <- system.file(
  "examples",
  "longbone_orientation_example.R",
  package = "OrientCSG"
)

file.edit(example_file)
```

To run it:

```r
source(example_file)
```

To inspect or run the mandibular example:

```r
mandible_example <- system.file(
  "examples",
  "mandible_orientation_example.R",
  package = "OrientCSG"
)

file.edit(mandible_example)
source(mandible_example)
```

A minimal TRUE-volume long-bone call now points OrientCSG directly to the local DICOM series. The automatic route requires the suggested package `oro.dicom` (install it once with `install.packages("oro.dicom")`):

```r
dicom_dir <- r"(D:\path\to\the\DICOM_series)"

res <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str,
  dicom_dir = dicom_dir,
  landmarks_str = landmarks_str,
  section_loc = 50,
  individual_id = "T108_Left",
  camera_distance = 1
)

res$summary
cat(get_tcl(res, section = "SECTION_50"))
```

On Windows, raw strings such as `r"(D:\path\to\folder)"` are recommended for file and directory paths. This allows paths copied directly from File Explorer to be pasted without escaping backslashes; paths using forward slashes also remain valid inside a raw string.

The automatic DICOM reader is internal rather than a separate public function. The IOP, IPP1, and IPP2 values actually used are written into the `Bio_Length_&_Orient` column of `res$summary`. For speed, only two readable headers are opened; the selected slices must have consecutive `InstanceNumber` values. If a directory does not meet that assumption, supply the established manual `dicom_orientation = c(IOP, IPP1, IPP2)` input instead.

For 3D Slicer workflows, use `SLICER = TRUE` and inspect the generated Python block with:

```r
cat(get_slicer_py(res, section = "SECTION_50"))
```

## Batch capture with `flash_capture()`

`flash_capture()` generates one batch command block for several long-bone sections. It currently supports the three primary long-bone capture routes:

- Avizo/Amira + CT (`SLICER = FALSE`, `SOLID = FALSE`);
- 3D Slicer + CT (`SLICER = TRUE`, `SOLID = FALSE`);
- 3D Slicer + SOLID mesh (`SLICER = TRUE`, `SOLID = TRUE`).

In 3D Slicer, run one reference section first, usually `SECTION_50`, and configure the desired view before using `flash_capture()`. In Avizo/Amira, no prior `copy_tcl()` step is required: Flash Capture initializes the first requested section with the current result's Slice and AP/ML planes and rotates the existing camera perpendicular to the section while preserving its position and zoom.

```r
# Prepare the reference section first:
copy_slicer_py(res, section = "SECTION_50")

# After pasting that block in Slicer and configuring the view:
flash_capture(
  res,
  output_dir = r"(C:\OrientCSG\captures)",
  sections = c(20, 35, 50, 65, 80)
)
```

If `file_name` is omitted, `res$individual_id` is used. The generated files are named with the requested section percentage, for example `T109_20.tif`, `T109_35.tif`, and `T109_50.tif`. RGB is the default capture mode. In 3D Slicer, `color_mode = "grayscale"` converts the rendered RGB buffer to a true one-channel luminance image and writes a lossless Deflate-compressed TIFF. In Avizo/Amira, the scripted `viewer snapshot` interface does not expose a reliable grayscale switch; requesting `color_mode = "grayscale"` therefore warns and falls back to the native RGB snapshot.

`viewer_id` and `reference_tolerance_mm` are no longer public arguments. Flash Capture uses Avizo/Amira viewer 0 and a fixed 2 mm Slicer reference-section tolerance internally.

In Avizo/Amira, Flash Capture initializes the first requested section using the orientation stored in `res`: it updates the Slice and AP/ML visual planes and rotates the existing camera perpendicular to the section once, while preserving camera position, projection type, and zoom/framing. It then reuses that camera while moving the Slice through the remaining sections. In Slicer CT, it translates the prepared slice view and OrientCSG scale while preserving orientation, field of view, pan, and display settings. In Slicer SOLID, it re-cuts the source mesh at each requested level and preserves the prepared 3D camera. The Slicer branches restore the starting reference view when the batch finishes.

## Working with Slicer Python output

When `SLICER = TRUE`, the generated Python blocks are stored in `res$slicer_py`.

For long-bone results, section names follow the requested section percentages, such as `"SECTION_35"` or `"SECTION_50"`. For mandibular results, section names are `"CS1"`, `"CS2"`, and `"CS3"`.

```r
names(res$slicer_py)

# Long-bone example
get_slicer_py(res, section = "SECTION_50")
copy_slicer_py(res, section = "SECTION_50")

# Mandibular example
get_slicer_py(res, section = "CS1")
copy_slicer_py(res, section = "CS1")
```

Paste the copied block into the 3D Slicer Python Interactor. Long-bone Slicer blocks define `restore_view()` as the main restoration command. In the solid-mesh route this restores the generated 3D verification view. In the TRUE-volume route it restores the Red slice orientation, the 3D verification camera, and the scale bar. TRUE-volume blocks also define `refresh_orientcsg_scale()` for recreating the 10 mm scale bar at the current slice position, and `restore_3d_camera()` if only the 3D camera needs to be restored.

For solid-mesh long-bone blocks:

```python
restore_view()
```

For TRUE-volume long-bone blocks:

```python
restore_view()
refresh_orientcsg_scale()
restore_3d_camera()
```

Mandibular Slicer blocks orient the Red slice view to the requested anatomical section of the loaded scalar volume. They also create an ARP plane, an `LM1_Line` fiducial, a 10 mm scale bar, and a 3D verification view in which the ARP appears horizontally edge-on and the section plane appears vertically edge-on. The generated block defines `restore_view()` and `refresh_orientcsg_scale()`. Run `restore_view()` in the Slicer Python Interactor to restore the original mandibular slice orientation, 3D verification view, and scale. Run `refresh_orientcsg_scale()` to recreate the 10 mm scale bar at the current slice position.

## Avizo/Amira requirements

The generated TCL code refers to Amira/Avizo objects by name. These objects must already exist in the project and their names must match exactly.

For mandibular workflows, the expected objects are:

- `ARP`: clipping plane used to display the alveolar reference plane;
- `Slice`: slice object used to display the active cross-section;
- `OrthogonalView`: optional clipping plane used as a visual check. The TCL code will still run if this object does not exist.

For long-bone workflows, the expected objects are:

- `Slice`: slice object used for the transverse section;
- `ML`: clipping plane used for the mediolateral anatomical plane;
- `AP`: clipping plane used for the anteroposterior anatomical plane.

## Common issues

Most errors or unexpected orientations are caused by one of the following problems:

- landmark coordinates were pasted in the wrong order;
- the wrong mandibular preservation option was selected (`complete_arch`, `estimate_lm10`, or `lm9_valid`);
- the wrong number of mandibular landmarks was supplied;
- the BoneJ Log/eigenvector input was copied incorrectly;
- the wrong DICOM Image Orientation (Patient) or Image Position (Patient) values were supplied, the IPP values were not taken from two consecutive slices in stack order, or the metadata came from a different stack than the one processed in BoneJ;
- the wrong long-bone mode was selected;
- `SOLID = TRUE` was requested but the mesh is not closed or cannot be read by `Rvcg`;
- the wrong coordinate convention was used for Slicer landmarks;
- the model name in Slicer does not match `model_name`;
- the required Amira/Avizo objects do not exist or have different names;
- `HUMERUS_TABLE` was used even though scan orientation was not anatomically standardized;
- `SLICER = TRUE` was requested with `HUMERUS_TABLE`, which is not supported;
- section names were typed incorrectly when using `get_tcl()`, `copy_tcl()`, `write_tcl()`, `get_slicer_py()`, `copy_slicer_py()`, or `flash_capture()`;
- in 3D Slicer, `flash_capture()` was run before a normal OrientCSG reference section had been pasted and configured.

## Utility function

OrientCSG also exports `dist3()`, a small utility for computing Euclidean distance between two 3D points:

```r
dist3(c(1, 2, 3), c(5, 5, 3))
```

The returned value is expressed in the same linear unit as the input coordinates.

## Development status

OrientCSG is under active methodological development.

Major milestones include v0.1.1, which introduced Avizo/Amira TCL generation for mandibular, tibial, and humeral workflows; v0.2.0, which added solid-mesh workflows and 3D Slicer output for long bones; and v0.3.0, which introduced the mandibular 3D Slicer backend. v1.0.0 expanded long-bone support to the femur and radius, while v1.0.1 established the current BoneJ-to-DICOM orientation workflow. v1.0.3 added flash_capture() for automated batch section export, and v1.0.4 introduced automatic DICOM metadata extraction and grayscale TIFF output in Slicer. Planned developments include workflows for fragmented long bones and extension to additional elements and preservation scenarios.

Planned developments include workflows for fragmented long bones and extension to additional elements and preservation scenarios.

## Methodological documentation

This README is intended as a practical guide to installing and running the package. It does not provide a full methodological justification of the geometric operations implemented in OrientCSG.

For correct use of the package, users should refer to the associated methodological publications and protocols, which describe the anatomical logic behind the reference systems, the rationale for each geometric decision, and the precision and error of the method.

Until these publications are available, OrientCSG should be treated as a research tool under active development.

For questions, contact `almartan@ucm.es`.

## Funding and support

Development of this package was supported by the FCT R&D research project “ParaFunction” (project reference 2022.07737.PTDC; https://doi.org/10.54499/2022.07737.PTDC).

## References

Ruff, C. B. (2002). Long bone articular and diaphyseal structure in old world monkeys and apes. I: Locomotor effects. *American Journal of Physical Anthropology*, *119*(4), 305–342. https://doi.org/10.1002/ajpa.10117

Toro-Ibacache, V., Ugarte, F., Morales, C., Eyquem, A., Aguilera, J., & Astudillo, W. (2019). Dental malocclusions are not just about small and weak bones: assessing the morphology of the mandible with cross-section analysis and geometric morphometrics. *Clinical Oral Investigations*, *23*(9), 3479–3490. https://doi.org/10.1007/s00784-018-2766-6
