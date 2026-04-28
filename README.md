# OrientCSG

OrientCSG is an R package for reproducible orientation of mandibular and long-bone cross-sections in cross-sectional geometry workflows. It was designed to support systematic virtual image capture from CT-derived anatomical data, with future extension to 3D scan-derived data currently under development The package is especially useful when sections must be oriented consistently across specimens before measuring biomechanical properties.

The package computes anatomical reference systems that allow transverse section planes to be oriented according to criteria grounded in Ruff’s (2002) proposals for long bones and broadly comparable to the mandibular orientation approach of Toro-Ibacache et al. (2019). It also computes section locations, orientation vectors, camera positions, summary tables, manual-orientation tables, and Amira/Avizo TCL command blocks.

## Installation

Install the package from GitHub with:

```r
install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG")
library(OrientCSG)
```

## What OrientCSG does

OrientCSG takes landmark coordinates and, for long bones, a BoneJ eigenvector matrix as input. It then returns an R object containing:

- computed anatomical points and vectors;
- section locations;
- summary tables;
- size-related measurements when applicable;
- manual-orientation tables;
- Avizo TCL command blocks.

The generated TCL blocks can be copied into the Avizo console or written to `.tcl` files.

## What OrientCSG does not do

OrientCSG does not directly control Avizo from R. It does not open Avizo projects, create Avizo objects, segment CT data, extract contours, or calculate cross-sectional geometry properties such as cortical area, total area, second moments of area, polar moment of area, or section modulus.

The package assumes that the relevant Avizo objects already exist and that landmark coordinates have been obtained beforehand.

## Current scope

OrientCSG currently supports two main workflows.

### 1. Mandibular cross-sections

The mandibular workflow is implemented in `orient_mandible()`.

It currently:

- defines the alveolar reference plane (ARP) from `LM1`, `LM2`, and `LM1_Line` -which can be estimated when the specimen is fragmented-;
- computes `CS1`, `CS2`, and `CS3` following the mandibular landmark protocol;
- returns summary tables, size-related mandibular measurements, manual-orientation tables, and one Avizo TCL block per section.

### 2. Long-bone cross-sections

The long-bone workflow is implemented in `orient_longbone()`.

It currently:

- uses the first column of the BoneJ Moments of Inertia eigenvector matrix as the longitudinal axis;
- applies the BoneJ-to-Avizo coordinate correction `(x, y, z) -> (-x, -y, z)`;
- supports `TIBIA`, `HUMERUS`, and `HUMERUS_TABLE` modes;
- computes biomechanical length, section locations, anatomical axes, manual-orientation tables, and Avizo TCL blocks for each requested section percentage.

The three supported modes are:

- `TIBIA`: uses two tibial plateau landmarks and one tibio-talar landmark.
- `HUMERUS`: uses four landmarks, including two distal landmarks for the mediolateral reference direction and two landmarks for biomechanical length.
- `HUMERUS_TABLE`: uses two landmarks for biomechanical length and derives the mediolateral direction from the scanner X axis. This mode should only be used when the humerus was scanned in a standardized table position where scan orientation carries anatomical information.

## Specimen preservation requirements

OrientCSG currently has different preservation requirements for mandibular and long-bone workflows.

For long bones, the current implementation assumes complete or near-complete specimens. This is because biomechanical length, section locations, and anatomical axes are computed from landmarks and reference directions that require preservation of the relevant proximal and distal anatomy. A version designed to orient fragmented long bones is currently under development, but it is not yet implemented.

For mandibles, the workflow can be applied to fragmented specimens provided that the anatomical region required by the landmark protocol is sufficiently preserved. In practical terms, this means that the specimen should preserve, at minimum, the region extending from the first incisor to the second molar (both included), together with the landmarks needed to define the mandibular reference system and the target cross-sections.

## Methodological documentation

This README is intended as a practical guide to installing and running the package. It does not provide a full methodological justification of the geometric operations implemented in OrientCSG. For correct use of the package, users should refer to the associated methodological publications and protocols, which describe the anatomical logic behind the reference systems, the rationale for each geometric decision, and the precision and error of the method.

At present, the mandibular workflow is being prepared for documentation in a protocols.io protocol and an associated methodological article. Equivalent methodological documentation for the long-bone workflows is also under development. Until these publications are available, OrientCSG should be treated as a research tool under active development. 

If you need guidance on how to use the package correctly before the associated methodological documentation is available, please contact `almartan@ucm.es`. A provisional tutorial template can be shared on request.

## Avizo requirements

The generated TCL code refers to Avizo objects by name. These objects must already exist in the project and their names must match exactly.

For mandibular workflows, the expected objects are:

- `ARP`: clipping plane used to display the alveolar reference plane;
- `Slice`: slice object used to display the active cross-section;
- `OrthogonalView`: optional clipping plane used as a visual check. The TCL code will still run if this object does not exist.

For long-bone workflows, the expected objects are:

- `Slice`: slice object used for the transverse section;
- `ML`: clipping plane used for the mediolateral anatomical plane;
- `AP`: clipping plane used for the anteroposterior anatomical plane.

## Basic mandibular workflow

The mandibular workflow expects 11 landmarks in the fixed protocol order. Coordinates can be pasted as a single character string copied from an Avizo landmark export or another coordinate source.

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
  cs3_camera_side = "RIGHT"
)

res
res$summary
res$measurements
res$manual_orientation
```

To retrieve a TCL block:

```r
cat(get_tcl(res, section = "CS1"))
```

To copy it to the clipboard:

```r
copy_tcl(res, section = "CS1")
```

To write it to disk:

```r
write_tcl(res, file = "MANDIBLE_001_CS1.tcl", section = "CS1")
```

## Basic long-bone workflow

The long-bone workflow requires a BoneJ eigenvector matrix and the anatomical landmarks required by the selected mode.

```r
library(OrientCSG)

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
  landmarks_str = tibia_landmarks_str,
  section_loc = 50,
  individual_id = "TIBIA_001",
  camera_distance_mm = 300
)

res
res$summary
res$manual_orientation
```

To retrieve the generated TCL block:

```r
cat(get_tcl(res, section = "SECTION_50"))
```

To write it to disk:

```r
write_tcl(res, file = "TIBIA_001_SECTION_50.tcl", section = "SECTION_50")
```

Multiple section locations can be requested at once:

```r
res <- orient_longbone(
  mode = "TIBIA",
  longitudinal_matrix_str = longitudinal_matrix_str,
  landmarks_str = tibia_landmarks_str,
  section_loc = c(35, 50, 65),
  individual_id = "TIBIA_001"
)

names(res$avizo_tcl)
```

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

## Working with TCL output

All orientation functions return an object of class `orientcsg_orientation`. The generated TCL blocks are stored in the object and can be accessed using the helper functions `get_tcl()`, `copy_tcl()`, and `write_tcl()`.

For mandibular workflows, valid section names are usually:

```r
c("CS1", "CS2", "CS3")
```

For long-bone workflows, section names follow the requested section percentages:

```r
c("SECTION_35", "SECTION_50", "SECTION_65")
```

For example:

```r
get_tcl(res, section = "SECTION_50")
copy_tcl(res, section = "SECTION_50")
write_tcl(res, file = "section_50.tcl", section = "SECTION_50")
```

## Common issues

Most errors or unexpected orientations are caused by one of the following problems:

- landmark coordinates were pasted in the wrong order;
- the wrong long-bone mode was selected;
- the required Avizo objects do not exist or have different names;
- `HUMERUS_TABLE` was used even though scan orientation was not anatomically standardized;
- section names were typed incorrectly when using `get_tcl()`, `copy_tcl()`, or `write_tcl()`.

## Utility function

OrientCSG also exports `dist3()`, a small utility for computing Euclidean distance between two 3D points:

```r
dist3(c(1, 2, 3), c(5, 5, 3))
```

The returned value is expressed in the same linear unit as the input coordinates.

## Development status

OrientCSG is under active methodological development. Version 0.1.0 focuses on Avizo TCL generation for mandibular, tibial, humeral, and table-position humeral workflows.

Future extensions may add alternatives for free and open-source software, including 3D Slicer and MeshLab-based surface mesh workflows. Planned developments also include support for additional long bones, particularly the femur (in the nearer term) and the radius (at a later stage), as well as protocols for orienting fragmented long-bone specimens.

## References

Ruff, C. B. (2002). Long bone articular and diaphyseal structure in Old World monkeys and apes. II: Estimation of body mass. *American Journal of Physical Anthropology*, *120(1)*, 16–37. https://doi.org/10.1002/ajpa.10118

Toro-Ibacache, V., Ugarte, F., Morales, C., Eyquem, A., Aguilera, J., & Astudillo, W. (2019). Dental malocclusions are not just about small and weak bones: assessing the morphology of the mandible with cross-section analysis and geometric morphometrics. *Clinical Oral Investigations*, *23(9)*, 3479–3490. https://doi.org/10.1007/s00784-018-2766-6
