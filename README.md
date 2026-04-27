# OrientCSG

OrientCSG is an R package for reproducible orientation of mandibular and long-bone cross-sections in cross-sectional geometry workflows. It was designed to support systematic virtual image capture from CT-derived anatomical data, especially when sections must be oriented consistently across specimens before measuring biomechanical properties.

The package computes anatomical reference systems, section locations, orientation vectors, camera positions, and Avizo/Amira TCL command blocks. The aim is not to replace anatomical judgement, but to reduce repeated manual operations, transcription errors, and small orientation inconsistencies during image capture.

## Current scope

OrientCSG currently supports two main workflows:

1. Mandibular cross-sections
   - Defines the alveolar reference plane (ARP) from LM1, LM2, and the estimated LM1_Line.
   - Computes CS1, CS2, and CS3 following the mandibular landmark protocol.
   - Returns summary tables, size-related mandibular measurements, manual-orientation tables, and one Avizo TCL block per section.

2. Long-bone cross-sections
   - Uses the first column of the BoneJ Moments of Inertia eigenvector matrix as the longitudinal axis.
   - Supports `TIBIA`, `HUMERUS`, and `HUMERUS_TABLE` modes.
   - Computes biomechanical length, section locations, anatomical axes, manual-orientation tables, and Avizo TCL blocks for each requested section percentage.

## Installation

During development, install the package from its local source directory or from GitHub once the repository is updated.

```r
# Local development use
# Run this from inside the package project

devtools::load_all()
```

If the package is hosted on GitHub, installation will follow the usual form:

```r
# Example only: replace user/repository with the final repository path
# remotes::install_github("user/OrientCSG")
```

## Basic mandibular workflow

The mandibular workflow expects 11 landmarks in the fixed order defined by the protocol. The input can be pasted as a single character string copied from an Avizo/Amira landmark export or another coordinate source.

```r
library(OrientCSG)

landmarks_str <- "
-30.80 -7.69 -143.70
 -1.58  7.37 -105.56
 -0.33 -22.72  -93.42
 -0.53 -16.99 -108.47
-28.76  -5.05 -132.70
-21.38  -4.79 -134.53
-25.12  -0.85 -121.62
-19.50  -0.58 -123.95
-46.74 -35.26 -164.05
-18.11   2.74 -110.29
-47.78  12.43 -201.18
"

res <- orient_mandible(
  landmarks_str = landmarks_str,
  individual_id = "MANDIBLE_001",
  camera_distance_mm = 300,
  cs3_camera_side = "RIGHT"
)

View(res$summary)
View(res$measurements)
cat(res$avizo_tcl$CS1)
```

The resulting object contains:

- `summary`: landmarks, computed points, and orientation vectors.
- `measurements`: linear mandibular measurements useful for size-related checks or standardization.
- `manual_orientation`: values arranged for manual verification in Avizo/Amira.
- `avizo_tcl`: named TCL blocks for `CS1`, `CS2`, and `CS3`.

To copy one block directly to the clipboard:

```r
copy_tcl(res, section = "CS1")
```

## Basic long-bone workflow

The long-bone workflow combines a BoneJ eigenvector matrix with anatomical landmarks. The first column of the BoneJ matrix is interpreted as the longitudinal vector and is transformed to the coordinate convention used by the Avizo/Amira command blocks.

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
  section_loc = c(50),
  individual_id = "TIBIA_001",
  camera_distance_mm = 300
)

View(res$summary)
View(res$manual_orientation)
cat(res$avizo_tcl$SECTION_50)
```

For humeri, use either `mode = "HUMERUS"` when the mediolateral direction is defined by landmarks, or `mode = "HUMERUS_TABLE"` when the bone was scanned in a standardized table position and the mediolateral axis should be derived from the scanner X axis.

## Avizo/Amira object names

The generated TCL code assumes that specific objects already exist in the Avizo/Amira project.

For mandibular workflows:

- `ARP`: Clipping Plane used to display the alveolar reference plane.
- `Slice`: Slice object used to display the active cross-section.
- `OrthogonalView`: optional Clipping Plane used as a visual check. The TCL code will still run if this object does not exist.

For long-bone workflows:

- `Slice`: Slice object used for the transverse section.
- `ML`: Clipping Plane used for the mediolateral anatomical plane.
- `AP`: Clipping Plane used for the anteroposterior anatomical plane.

Object names must match exactly, because the TCL blocks refer to these names directly.

## Utility function

OrientCSG also exports `dist3()`, a small utility for computing Euclidean distance between two 3D points:

```r
dist3(c(1, 2, 3), c(5, 5, 3))
```

The returned value is expressed in the same linear unit as the input coordinates.

## Development status

The package is under active methodological development. The current implementation focuses on Avizo/Amira TCL generation. Future extensions may add alternative export backends for 3D Slicer, MeshLab, or mesh-based sectioning workflows.
