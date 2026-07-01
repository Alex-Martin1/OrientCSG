# OrientCSG

OrientCSG is an R package for reproducible orientation of mandibular and long-bone cross-sections in cross-sectional geometry workflows.

The package was designed to generate consistent anatomical reference systems for virtual section capture. It supports three broad types of workflows:

1. classic CT-derived workflows using BoneJ-derived principal axes and Amira/Avizo TCL output;
2. solid surface mesh workflows using `.ply`, `.stl`, or `.obj` files, with optional 3D Slicer Python output; and
3. mandibular volume workflows with either Avizo/Amira TCL or 3D Slicer Python output.

OrientCSG computes section locations, anatomical vectors, camera/view parameters, summary tables, manual-orientation tables, Amira/Avizo TCL command blocks, and, where requested, 3D Slicer Python blocks.

The anatomical logic for long bones follows criteria grounded in Ruff’s (2002) proposals for long-bone orientation. The mandibular workflow is broadly comparable to the mandibular orientation approach of Toro-Ibacache et al. (2019).

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG")
library(OrientCSG)
```

The solid mesh workflow uses `Rvcg` when `SOLID = TRUE`. If it is not already installed, install it with:

```r
install.packages("Rvcg")
```

## What OrientCSG does

OrientCSG takes anatomical landmark coordinates and computes reproducible reference systems for cross-section orientation.

Depending on the workflow, it can:

- compute anatomical points and vectors;
- compute section locations;
- compute long-bone longitudinal axes from either a BoneJ eigenvector matrix or a closed surface mesh;
- return summary tables and manual-orientation tables;
- generate Amira/Avizo TCL command blocks;
- generate 3D Slicer Python blocks for supported Slicer workflows;
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
- `HUMERUS_TABLE`.

For classic CT-derived workflows, `orient_longbone()` uses the first column of the BoneJ Moments of Inertia eigenvector matrix as the longitudinal axis. The BoneJ matrix is now transformed using the DICOM Image Orientation (Patient) line (`0020,0037`) from the same image stack used in BoneJ. This avoids assuming a single fixed BoneJ-to-Avizo correction for all DICOM series.

For closed surface meshes, `orient_longbone()` can compute the longitudinal axis directly from the mesh when `SOLID = TRUE`. The mesh is treated as a homogeneous closed solid, and the eigenvector associated with the smallest principal moment of inertia is used as the longitudinal axis.

When `SLICER = TRUE`, OrientCSG generates a 3D Slicer Python block that creates the oriented section and sets the 3D view. This Slicer workflow is currently implemented for:

- `TIBIA`;
- `HUMERUS`;
- `FEMUR`;
- `RADIUS`.

It is intentionally not implemented for `HUMERUS_TABLE`, because that mode depends on a standardized scanner/table orientation that is usually not preserved in free 3D surface scanning workflows.

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

For classic BoneJ workflows, the landmark coordinate system and the BoneJ matrix transformation are separate issues. `lm_coord_system` only controls the landmarks. The BoneJ eigenvectors are transformed with `dicom_iop` by default. For example, a DICOM line such as `-1\0\0\0\-1\0` reproduces the previous `(-x, -y, z)` correction, whereas `1\0\0\0\-1\0` gives a different transformation. The result object stores this information in `res$bonej` and reports a simple longitudinal-axis check in `res$longitudinal_axis_check`.

## Preservation requirements

For long bones, OrientCSG currently assumes complete or near-complete specimens. Biomechanical length, section locations, and anatomical axes require preservation of the relevant proximal and distal anatomy.

For mandibles, the workflow can be applied to fragmented specimens if the anatomical region required by the landmark protocol is sufficiently preserved. The function accepts 9, 11, or 12 landmarks. The 9-landmark input is intended for cases where `LM10` and `LM11` cannot be placed. The 12-landmark input adds `LM12` as the contralateral gonion, allowing direct computation of bigonial breadth.

## Basic mandibular workflow

The mandibular workflow accepts 9, 11, or 12 landmarks in the fixed protocol order. Coordinates can be pasted as a single character string copied from an Avizo landmark export, a 3D Slicer Markups table, or another coordinate source. The coordinate system is declared separately with `lm_coord_system`, whose default is `"LPS"`.

`orient_mandible()` uses the landmarks to fix both the cross-section planes and the viewing orientation. The ARP normal (`Vec_Penp`) is signed anatomically from inferior toward superior. The priority is: real `LM9` when the gonion is preserved and `lm9_valid = TRUE`; `LM3`/`LM4` when they provide the appropriate inferior reference for the selected protocol; and an orientation-only `LM9` placeholder when the real gonion is not preserved and `lm9_valid = FALSE`. This same signed vector is used for both Avizo/Amira TCL and 3D Slicer Python outputs.

Use `lm1_side` to declare the anatomical side on which `LM1` was placed. The default is `"RIGHT"`. If the right corpus is poorly preserved and `LM1` is placed on the left side, set `lm1_side = "LEFT"`; OrientCSG then uses the left-side convention for CS1/CS2 viewing and for the anatomical transverse direction used by CS3 in both Avizo/Amira and 3D Slicer outputs.

```r
library(OrientCSG)

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
  individual_id = "MANDIBLE_001",
  camera_distance_mm = 300,
  lm1_side = "RIGHT"
)

res$summary
res$measurements
res$manual_orientation
```

Retrieve or copy an Amira/Avizo TCL block:

```r
cat(get_tcl(res, section = "CS1"))
copy_tcl(res, section = "CS1")
write_tcl(res, file = "MANDIBLE_001_CS1.tcl", section = "CS1")
```

For 3D Slicer, use the same landmarks and set `SLICER = TRUE`. If the landmark text was copied from a Slicer Markups table or exported from Slicer in the usual Markups workflow, keep `lm_coord_system = "LPS"` unless you have verified that the copied text is true RAS. Use `lm_coord_system = "RAS"` only for coordinates extracted explicitly as Slicer world RAS, for example with `GetNthControlPointPositionWorld()`. Set `volume_name` to the scalar volume node name in Slicer when more than one volume may be loaded.

```r
res_slicer <- orient_mandible(
  landmarks_str = landmarks_str,
  individual_id = "MANDIBLE_001",
  lm1_side = "RIGHT",
  lm_coord_system = "LPS",
  SLICER = TRUE,
  volume_name = "MANDIBLE_VOLUME"
)

cat(get_slicer_py(res_slicer, section = "CS1"))
copy_slicer_py(res_slicer, section = "CS1")
```

### Mandibular preservation options

By default, `orient_mandible()` assumes the fragmented-mandible workflow:

```r
res <- orient_mandible(
  landmarks_str = landmarks_str,
  complete_arch = FALSE,
  estimate_lm10 = FALSE,
  lm9_valid = TRUE
)
```

If the mandibular arch is sufficiently complete and the physical `LM1_Line`/`A_Line` point is preserved, set `complete_arch = TRUE`. In this mode, `LM4` is interpreted as the real `LM1_Line` point rather than the landmark used to estimate it by reflection:

```r
res <- orient_mandible(
  landmarks_str = landmarks_str,
  complete_arch = TRUE
)
```

If the contralateral gonion is preserved, provide 12 landmarks. `LM12` will be interpreted as the contralateral gonion and bigonial breadth will be computed directly as `LM9`--`LM12`.

If `LM10` and `LM11` cannot be placed, provide only the first 9 landmarks. The package will still generate the section-orientation TCL blocks, but mandibular length will be returned as non-computable in `res$measurements`.

If `LM9` cannot be placed on a real anatomical gonion and a placeholder is used only to preserve the input structure, set `lm9_valid = FALSE`. In this case, measurements depending on `LM9`, including corpus length and bigonial breadth, are returned as non-computable.

## Classic long-bone workflow: BoneJ + Amira/Avizo

The classic long-bone workflow requires:

1. the DICOM Image Orientation (Patient) line, copied from the same image stack used in BoneJ;
2. a BoneJ Moments of Inertia eigenvector matrix; and
3. the anatomical landmarks required by the selected mode.

Use a raw R string, `r"(...)"`, when pasting the DICOM line so that the backslashes are read literally.

```r
library(OrientCSG)

dicom_iop_str <- r"(0020,0037 Image Orientation (Patient): -1\0\0\0\-1\0)"

longitudinal_matrix_str <- "
||0.008|-0.758|-0.653||
||0.017|-0.652|0.758||
||1.000|0.017|-0.008||
"

tibia_landmarks_str <- "
130.94606  -12.514749 -392.244507
164.351898 -17.573267 -395.017944
146.258621 -15.388991  -61.599937
"

res <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str,
  dicom_iop = dicom_iop_str,
  landmarks_str = tibia_landmarks_str,
  section_loc = 50,
  individual_id = "TIBIA_001",
  camera_distance_mm = 300
)

res$summary
res$manual_orientation
```

Retrieve or copy the generated TCL block:

```r
cat(get_tcl(res, section = "SECTION_50"))
copy_tcl(res, section = "SECTION_50")
write_tcl(res, file = "TIBIA_001_SECTION_50.tcl", section = "SECTION_50")
```

Multiple section locations can be requested at once:

```r
res <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str,
  dicom_iop = dicom_iop_str,
  landmarks_str = tibia_landmarks_str,
  section_loc = c(35, 50, 65),
  individual_id = "TIBIA_001"
)

names(res$avizo_tcl)
```

## Solid mesh + 3D Slicer workflow

For closed surface meshes (`.ply`, `.stl`, or `.obj`), `orient_longbone()` can compute the longitudinal axis directly from the mesh.

This workflow is activated with:

```r
SOLID = TRUE
```

If `SLICER = TRUE`, OrientCSG also generates a Python block for 3D Slicer. The block can be pasted into the 3D Slicer Python Interactor to create the oriented section, set the 3D view, display the axes, and configure the view for capture. The generated output is emitted to Slicer in RAS world coordinates; therefore OrientCSG converts from its internal LPS/file-space convention to RAS only at this output step.

### Tibia example

```r
library(OrientCSG)

mesh_file_tibia <- "C:/Users/Alex/Desktop/T108_Left_solid.ply"

landmarks_str_slicer_tibia <- "
1 164.351898 -17.573267 -395.017944 0 0 0 1 1 1 0 F-1 2 0
2 130.946060 -12.514749 -392.244507 0 0 0 1 1 1 0 F-2 2 0
3 146.258621 -15.388991  -61.599937 0 0 0 1 1 1 0 F-3 2 0
"

res_tibia_solid_slicer <- orient_longbone(
  mode = "TIBIA",
  mesh_file = mesh_file_tibia,
  landmarks_str = landmarks_str_slicer_tibia,
  lm_coord_system = "LPS",
  section_loc = 50,
  individual_id = "T108_Left",
  model_name = "T108_Left_solid",
  SOLID = TRUE,
  SLICER = TRUE
)

res_tibia_solid_slicer$summary
res_tibia_solid_slicer$mesh_axes$eigenvectors

cat(get_slicer_py(res_tibia_solid_slicer, "SECTION_50"))
copy_slicer_py(res_tibia_solid_slicer, "SECTION_50")
```

The current tibia/Slicer parser expects Slicer Markups-style rows in this fixed order. If these rows were copied from the Slicer table, use `lm_coord_system = "LPS"` unless the copied text has been verified as true RAS:

```text
row 1 = Plateau2
row 2 = Plateau1
row 3 = TibioTalar
```

### Humerus example

```r
library(OrientCSG)

mesh_file_humerus <- "C:/Users/Alex/Desktop/H108_Right_solid.ply"

landmarks_str_slicer_humerus <- "
1 164.789749145508 -15.670039176941 -68.205650329590 0 0 0 1 1 1 0 F-1 2 0
2 186.393386840820 -15.760459899902 -68.102157592773 0 0 0 1 1 1 0 F-2 2 0
3 182.241800000000  -6.976971000000 -59.921390000000 0 0 0 1 1 1 0 F-3 2 0
4 182.721400000000   8.127365000000 -345.482760000000 0 0 0 1 1 1 0 F-4 2 0
"

res_humerus_solid_slicer <- orient_longbone(
  mode = "HUMERUS",
  mesh_file = mesh_file_humerus,
  landmarks_str = landmarks_str_slicer_humerus,
  lm_coord_system = "LPS",
  section_loc = c(35, 50),
  individual_id = "H108_Right",
  model_name = "H108_Right_solid",
  SOLID = TRUE,
  SLICER = TRUE
)

res_humerus_solid_slicer$summary
res_humerus_solid_slicer$mesh_axes$eigenvectors

cat(get_slicer_py(res_humerus_solid_slicer, "SECTION_35"))
cat(get_slicer_py(res_humerus_solid_slicer, "SECTION_50"))

copy_slicer_py(res_humerus_solid_slicer, "SECTION_35")
copy_slicer_py(res_humerus_solid_slicer, "SECTION_50")
```

The current humerus/Slicer parser expects Slicer Markups-style rows in this fixed order. If these rows were copied from the Slicer table, use `lm_coord_system = "LPS"` unless the copied text has been verified as true RAS:

```text
row 1 = MedialTrocleaAnt
row 2 = CapitulumAnt
row 3 = LateralTrocleaDist
row 4 = ProximalHead
```

The femur and radius modes use the following landmark orders:

```text
FEMUR
row 1 = Condyle1
row 2 = Condyle2
row 3 = SuperiorNeck

RADIUS
row 1 = RadialStyloid
row 2 = UlnarNotch
row 3 = DistArticular
row 4 = ProxArticular
```

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

## Installed examples

OrientCSG includes example scripts that can be inspected or executed after installation:

```r
system.file("examples", package = "OrientCSG")
list.files(system.file("examples", package = "OrientCSG"))
```

To run the mandibular example:

```r
source(system.file("examples", "mandible_orientation_example.R", package = "OrientCSG"))
```

To run the long-bone example:

```r
source(system.file("examples", "longbone_orientation_example.R", package = "OrientCSG"))
```

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
- the BoneJ eigenvector matrix was copied incorrectly;
- the wrong DICOM Image Orientation (Patient) line was supplied, or it came from a different stack than the one processed in BoneJ;
- the wrong long-bone mode was selected;
- `SOLID = TRUE` was requested but the mesh is not closed or cannot be read by `Rvcg`;
- the wrong coordinate convention was used for Slicer landmarks;
- the model name in Slicer does not match `model_name`;
- the required Amira/Avizo objects do not exist or have different names;
- `HUMERUS_TABLE` was used even though scan orientation was not anatomically standardized;
- `SLICER = TRUE` was requested with `HUMERUS_TABLE`, which is not supported;
- section names were typed incorrectly when using `get_tcl()`, `copy_tcl()`, `write_tcl()`, `get_slicer_py()`, or `copy_slicer_py()`.

## Utility function

OrientCSG also exports `dist3()`, a small utility for computing Euclidean distance between two 3D points:

```r
dist3(c(1, 2, 3), c(5, 5, 3))
```

The returned value is expressed in the same linear unit as the input coordinates.

## Development status

OrientCSG is under active methodological development.

Version 1.0.0 adds femoral and radial long-bone modes for Avizo/Amira TCL and 3D Slicer Python workflows, including projected biomechanical-length calculation and distal-to-proximal axis checks for both elements. Version 0.3.3 clarifies Slicer coordinate handling: coordinates copied/exported from Slicer Markups may paste as LPS even when the interface displays R/A/S columns, whereas explicitly extracted world coordinates should be treated as RAS. It also fixes the tibial longitudinal-axis sign so tibial mesh workflows use a distal-to-proximal axis, and it orients the mandibular ARP normal anatomically from inferior toward superior for both Avizo/Amira and Slicer outputs. Version 0.3.1 updates the mandibular 3D Slicer backend so that in-plane slice orientation is defined anatomically: the screen vertical axis is now derived from the ARP normal projected into the section plane, forcing the ARP to appear horizontal in the captured slice. This improves agreement with the Amira/Avizo-oriented section views. Version 0.3.0 added the validated 3D Slicer backend for mandibular volume workflows. The generated mandibular blocks orient CS1, CS2, and CS3 in the Red slice view, create ARP and `LM1_Line` verification objects, use the `CT-AAA2` volume-rendering preset, provide a 3D verification view, and include `restore_view()` and `refresh_orientcsg_scale()` helper commands.

Version 0.2.0 added the solid surface mesh workflow and 3D Slicer Python output for tibial and humeral sections.

Version 0.1.4 introduced the first solid mesh + Slicer workflow for tibial sections. Version 0.1.2 updated mandibular TCL generation so that CS1, CS2, and CS3 are emitted as normal-and-point Slice definitions, improving compatibility across Amira/Avizo versions while preserving the same orientation geometry. Version 0.1.1 introduced Avizo TCL generation for mandibular, tibial, humeral, and table-position humeral workflows, including 9-, 11-, and 12-landmark mandibular inputs and explicit measurement status/method metadata.

Planned developments include protocols for orienting fragmented long-bone specimens and possible extension to additional elements or preservation scenarios.

## Methodological documentation

This README is intended as a practical guide to installing and running the package. It does not provide a full methodological justification of the geometric operations implemented in OrientCSG.

For correct use of the package, users should refer to the associated methodological publications and protocols, which describe the anatomical logic behind the reference systems, the rationale for each geometric decision, and the precision and error of the method.

Until these publications are available, OrientCSG should be treated as a research tool under active development.

For questions, contact `almartan@ucm.es`.

## Funding and support

Development of this package was supported by the FCT R&D research project “ParaFunction” (project reference 2022.07737.PTDC; https://doi.org/10.54499/2022.07737.PTDC).

## References

Ruff, C. B. (2002). Long bone articular and diaphyseal structure in Old World monkeys and apes. II: Estimation of body mass. *American Journal of Physical Anthropology*, *120*(1), 16–37. https://doi.org/10.1002/ajpa.10118

Toro-Ibacache, V., Ugarte, F., Morales, C., Eyquem, A., Aguilera, J., & Astudillo, W. (2019). Dental malocclusions are not just about small and weak bones: assessing the morphology of the mandible with cross-section analysis and geometric morphometrics. *Clinical Oral Investigations*, *23*(9), 3479–3490. https://doi.org/10.1007/s00784-018-2766-6
