# OrientCSG

OrientCSG is an R package for reproducible orientation of mandibular and long-bone cross-sections in cross-sectional geometry workflows.

## Overview

The package was designed to generate consistent anatomical reference systems for virtual section capture. It supports three broad workflow families:

1. volume-input long-bone workflows using BoneJ-derived principal axes, with Amira/Avizo TCL or 3D Slicer Python output;
2. mesh-input long-bone workflows using `.ply`, `.stl`, or `.obj` files, with 3D Slicer Python output; and
3. mandibular volume workflows with either Amira/Avizo TCL or 3D Slicer Python output.

The anatomical logic for long bones follows criteria grounded in Ruff's (2002) proposals for long-bone orientation. The mandibular workflow is broadly comparable to the mandibular orientation approach of Toro-Ibacache et al. (2019).

### What OrientCSG does

Depending on the workflow, OrientCSG can:

- compute anatomical points and vectors;
- compute section locations;
- compute long-bone longitudinal axes from a direct BoneJ longitudinal vector, current BoneJ Log output copied verbatim, a legacy 3 x 3 eigenvector matrix, a full BoneJ Results-table row, or a surface mesh;
- return summary tables and manual-orientation tables;
- generate Amira/Avizo TCL command blocks;
- generate 3D Slicer Python blocks for supported workflows;
- generate batch capture blocks with `flash_capture()`;
- generate an oriented 3D Slicer ROI for preliminary cropping of fragmented surface meshes with `get_fragmented_roi()`;
- generate whole-bone reorientation code with `Reorient()`; and
- copy generated command blocks to the clipboard.

### What OrientCSG does not do

OrientCSG does not directly control Amira/Avizo or 3D Slicer from R. It generates command blocks that the user can paste into the relevant software.

OrientCSG does not segment CT data, choose CT thresholds, extract contours, or calculate cross-sectional geometry properties such as cortical area, total area, second moments of area, polar moment of area, or section modulus.

For `INPUT = "VOLUME"`, the BoneJ-derived longitudinal axis necessarily reflects the external segmentation or binarization used to calculate it, while the section generated from the original volume can remain scalar/grayscale for later thresholding or segmentation. These decisions remain explicit and user-controlled.

## Installation

Install the current stable version from GitHub:

```r
install.packages("remotes")
remotes::install_github("Alex-Martin1/OrientCSG")
library(OrientCSG)
```

The mesh-input workflow uses `Rvcg` when `INPUT = "MESH"`. Automatic DICOM metadata extraction through `dicom_dir` uses `oro.dicom`. Install either suggested package when you need that workflow:

```r
install.packages("Rvcg")
install.packages("oro.dicom")
```

## Core concepts

### Input representation is not section type

`INPUT` describes the representation supplied to `orient_longbone()`; it does not describe whether the eventual CSG section is TRUE or SOLID.

`INPUT = "VOLUME"` is used for scalar volumetric data such as medical CT. The longitudinal axis comes from BoneJ and DICOM orientation metadata, while the generated section retains scalar/grayscale information. The section can later be thresholded or segmented to retain periosteal and endosteal information (TRUE), or, if required, converted to a filled periosteal-envelope representation (SOLID).

`INPUT = "MESH"` is used for a surface mesh. The longitudinal axis is calculated directly from mesh volumetric inertia and the Slicer section is geometric. A mesh from a surface scanner may encode only the periosteal envelope and therefore support a SOLID-style section, whereas a mesh derived from 3D segmentation may encode both periosteal and endosteal surfaces and support a TRUE-style section.

Neither `"VOLUME"` nor `"MESH"` should therefore be interpreted as synonymous with TRUE or SOLID.

### Longitudinal-axis estimation

For `INPUT = "VOLUME"`, `orient_longbone()` accepts the BoneJ longitudinal direction in four forms:

- three direct vector components;
- current BoneJ Log output copied directly from the Log window;
- the legacy compact 3 x 3 Moments of Inertia eigenvector matrix; or
- a full BoneJ Results-table row containing the unit-vector columns.

A direct three-component input is treated as the first BoneJ vector. For Log, matrix, or table input, the first vector is used as the longitudinal axis.

Current BoneJ Log output can be pasted without editing, for example:

```r
longitudinal_matrix_str <- "
[INFO] ||0.018|-0.826|-0.563||
[INFO] ||-0.019|0.562|-0.827||
[INFO] ||-1.000|-0.026|0.005||
"
```

The `[INFO]` prefixes and pipe characters are ignored by the parser; the three rows are interpreted as the 3 x 3 eigenvector matrix exactly as printed by BoneJ.

For routine volume-input analyses, the recommended workflow is to supply `dicom_dir`, pointing to the local DICOM directory used to build the ImageJ/BoneJ stack. OrientCSG reads only enough files to obtain two readable DICOM headers, verifies that their `InstanceNumber` values are consecutive, orders the pair by `InstanceNumber`, and derives Image Orientation (Patient) plus the ordered Image Position (Patient) pair automatically. Pixel data are not loaded.

Manual `dicom_orientation = c(IOP, IPP1, IPP2)` remains available for legacy or validation workflows. IOP defines the in-plane axes, while the ordered IPP pair determines the sign of the stack Z axis. The IOP/IPP values actually used are reported in `res$summary`, with additional transformation diagnostics available in `res$bonej`.

For `INPUT = "MESH"`, the longitudinal axis is calculated directly from the surface mesh using signed-tetrahedron volumetric integration. The mesh is treated as a homogeneous solid approximation, and the eigenvector associated with the smallest principal moment of inertia is used as the longitudinal axis.

Strict topological watertightness is not required. Small topological discontinuities or scan holes are generally unlikely to materially affect longitudinal-axis estimation, whereas large or strongly asymmetric openings or missing regions may bias the signed volume, centroid, inertia tensor, and longitudinal axis. Substantially incomplete meshes should therefore be inspected, repaired, or independently validated before analysis.

### Coordinate conventions

OrientCSG distinguishes the text format used to supply coordinates from the coordinate system represented by the numbers.

3D Slicer works internally in RAS coordinates, but Markups coordinates copied from the table or exported to common Markups files may be written as LPS. This can happen even when the Slicer table displays R/A/S column labels. `lm_coord_system` must describe the numeric values that actually arrive in R.

As a practical rule:

- coordinates copied manually from a Slicer Markups table, or exported from Slicer Markups without verifying the header, should usually be treated as `lm_coord_system = "LPS"`;
- coordinates extracted explicitly as Slicer world coordinates with Python, for example with `GetNthControlPointPositionWorld()`, should be treated as `lm_coord_system = "RAS"`.

RAS and LPS differ by the sign of X and Y:

```text
x_RAS = -x_LPS
y_RAS = -y_LPS
z_RAS =  z_LPS
```

If true Slicer world RAS coordinates are required, they can be extracted in the Slicer Python Interactor:

```python
markupsNode = slicer.util.getNode("NAME_OF_MARKUPS_NODE")
for i in range(markupsNode.GetNumberOfControlPoints()):
    p = [0.0, 0.0, 0.0]
    markupsNode.GetNthControlPointPositionWorld(i, p)
    label = markupsNode.GetNthControlPointLabel(i)
    print(i + 1, label, p[0], p[1], p[2])
```

### Preservation requirements

Long-bone workflows assume complete or near-complete specimens because biomechanical length, section locations, and anatomical axes require preservation of the relevant proximal and distal anatomy.

Mandibular workflows can accommodate different preservation states through 9-, 11-, and 12-landmark inputs, provided that the anatomical regions required by the selected protocol options are sufficiently preserved.

## Mandibular workflow

The mandibular workflow is implemented in `orient_mandible()`. It receives mandibular landmark coordinates, reconstructs the geometric reference system, computes three cross-sections, and returns tabular results plus software-specific command blocks.

### Landmarks and preservation modes

The function accepts 9, 11, or 12 landmarks in the fixed order defined by the mandibular protocol. Landmark identity is determined by input order rather than inferred from anatomical position.

| Order | Role in the workflow |
| --- | --- |
| LM1 | Preserved-side landmark used with LM2 and `LM1_Line` to define the alveolar reference plane |
| LM2 | Midline/reference landmark used in the ARP and as the point through which CS3 passes |
| LM3 | Reference landmark used in the reflection geometry for fragmented mandibles |
| LM4 | Reference landmark used in the reflection geometry; in `complete_arch = TRUE`, interpreted as the preserved `LM1_Line` / `A_Line` point |
| LM5-LM6 | Pair defining the direction used for CS1 |
| LM7-LM8 | Pair defining the direction used for CS2 |
| LM9 | Gonion when anatomically valid; may instead be treated as an orientation-only placeholder |
| LM10-LM11 | Pair used for mandibular length when available |
| LM12 | Contralateral gonion in the 12-landmark workflow, allowing direct bigonial breadth |

The 11-landmark input preserves the original workflow. The 12-landmark input adds LM12. The 9-landmark input is intended for specimens where LM10 and LM11 cannot be placed; mandibular length is then returned as non-computable.

By default, `complete_arch = FALSE` and `LM1_Line` is estimated by reflecting LM1 across the plane defined by LM2, LM3, and LM4. With `complete_arch = TRUE`, LM4 is interpreted as the physically preserved `LM1_Line` / `A_Line` point.

`estimate_lm10 = TRUE` allows LM10 to be reflected for mandibular-length estimation. `lm9_valid = FALSE` treats LM9 as a placeholder used only to preserve the input structure and suppresses measurements that require a real gonion.

### Reference system and sections

The alveolar reference plane (ARP) is defined from LM1, LM2, and `LM1_Line`.

`CS1` follows the direction LM5 to LM6 and is made perpendicular to the ARP. `CS2` follows the same logic using LM7 to LM8. `CS3` passes through LM2 and uses the LM1 to `LM1_Line` direction as its normal.

The function also computes auxiliary points and vectors used for manual verification, image orientation, and size-related measurements. The superoinferior reference is resolved anatomically, and the LM0 to LM2 direction is used to select the anterior viewing side for CS1 and CS2.

### Outputs

`orient_mandible()` returns:

- a compact coordinate/vector summary;
- mandibular size-related measurements with `status` and `method` metadata;
- manual-orientation output for Avizo/Amira;
- one TCL block per section when `SLICER = FALSE`; or
- one 3D Slicer Python block per section when `SLICER = TRUE`.

The Slicer workflow orients the selected slice view to CS1, CS2, or CS3, activates a CT volume-rendering preset when available, creates ARP and `LM1_Line` verification objects, adds a 10 mm scale bar hidden by default, and configures a 3D verification view.

## Long-bone workflow

The long-bone workflow is implemented in `orient_longbone()` and currently supports `TIBIA`, `HUMERUS`, `FEMUR`, `RADIUS`, `ULNA`, and the special `HUMERUS_TABLE` mode.

For the five anatomical modes, the longitudinal axis comes from BoneJ plus DICOM orientation metadata when `INPUT = "VOLUME"` or directly from mesh volumetric inertia when `INPUT = "MESH"`. The longitudinal vector is then signed distal-to-proximal using the relevant biomechanical-length landmarks.

The transverse landmark pair is projected onto the plane perpendicular to the longitudinal axis to construct an orthogonal ML/AP-oriented pair. `HUMERUS` and `ULNA` contain additional landmark geometry that resolves anatomical anterior. `TIBIA`, `FEMUR`, and `RADIUS` do not.

`section_loc` gives section position as a percentage of biomechanical length. For example:

```r
section_loc = c(35, 50)
```

generates `SECTION_35` and `SECTION_50`.

### TIBIA

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `Plateau1` | First tibial plateau landmark; paired with LM2 to define the transverse reference |
| LM2 | `Plateau2` | Second tibial plateau landmark; paired with LM1 to define the transverse reference |
| LM3 | `TibioTalar` | Distal tibio-talar landmark and distal endpoint of biomechanical length |

#### Orientation and biomechanical length

LM1 and LM2 define an anatomically undirected transverse axis. Their midpoint defines the proximal endpoint of biomechanical length after projection onto the longitudinal axis. LM3 defines the distal endpoint.

No supplied tibial landmark resolves anatomical anterior/posterior sign. In volume-input workflows, OrientCSG applies its established acquisition/coordinate convention after DICOM transformation to choose a reproducible AP-oriented sign. In mesh-input workflows, the transverse sign remains dependent on the input coordinate convention.

### HUMERUS

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `MedialTrocleaAnt` | Distal anterior landmark used with LM2 to define the transverse reference |
| LM2 | `CapitulumAnt` | Distal anterior landmark used with LM1 to define the transverse reference |
| LM3 | `LateralTrocleaDist` | Distal landmark used for biomechanical length and AP sign resolution |
| LM4 | `Proximal Head` | Proximal landmark used for biomechanical length |

#### Orientation and biomechanical length

LM1 and LM2 define the transverse reference. LM3 to the midpoint of LM1 and LM2 provides a posterior-to-anterior anatomical reference: LM1 and LM2 are expected to lie anterior to LM3.

Biomechanical length is the projected distance along the longitudinal axis from distal LM3 to proximal LM4.

### FEMUR

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `Condyle1` | Geometric centre of one distal condylar articular surface |
| LM2 | `Condyle2` | Geometric centre of the opposite distal condylar articular surface |
| LM3 | `SuperiorNeck` | Deepest and most distal point on the superior femoral-neck surface |

#### Orientation and biomechanical length

LM1 and LM2 define an anatomically undirected transverse axis. Their midpoint defines the distal endpoint of biomechanical length, while LM3 defines the proximal endpoint after projection onto the longitudinal axis.

No supplied femoral landmark resolves anatomical anterior/posterior sign. In volume-input workflows the final sign therefore follows the acquisition/coordinate convention; in mesh-input workflows it remains dependent on the mesh coordinate convention.

### RADIUS

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `RadialStyloid` | Most lateral point on the radial styloid tip |
| LM2 | `UlnarNotch` | Midpoint of the ulnar-notch border |
| LM3 | `DistArticular` | Geometric centre of the distal radiocarpal articular surface |
| LM4 | `ProxArticular` | Geometric centre of the proximal radial-head articular surface |

#### Orientation and biomechanical length

LM1 and LM2 define an anatomically undirected transverse axis. Biomechanical length is the projected distance along the longitudinal axis from distal LM3 to proximal LM4.

No supplied radial landmark resolves anatomical anterior/posterior sign. In volume-input workflows the final sign follows the acquisition/coordinate convention; in mesh-input workflows it remains dependent on the mesh coordinate convention.

### ULNA

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `TrochlearWaistLat` | Lateral point of the narrowest waist of the trochlear notch, placed on the trochlear articular edge |
| LM2 | `TrochlearWaistMed` | Medial point of the narrowest trochlear waist; if the articular edge disappears at the waist, use the AP depth indicated by the adjacent trochlear articular edge immediately proximal to the waist |
| LM3 | `RadialTrochlearBorder` | Point on the border between the radial and trochlear articular surfaces, placed as far toward the centre of the overall olecranon articular surface as possible while remaining on the border |
| LM4 | `UlnarHeadDistal` | Most distal point of the articular portion of the ulnar head, excluding the styloid process |

#### Orientation and biomechanical length

LM1 and LM2 are interchangeable and define the transverse reference. LM2 to LM3 provides the posterior-to-anterior cue used to resolve AP sign; LM3 is also required to lie anterior to LM1.

Biomechanical length is the projected distance along the longitudinal axis from distal LM4 to proximal LM3.

### HUMERUS_TABLE

`HUMERUS_TABLE` is a special acquisition-dependent mode intended for humeri scanned in a standardized table position.

#### Landmarks

| Order | Name | Definition / role |
| --- | --- | --- |
| LM1 | `LateralTrocleaDist` | Distal endpoint used for biomechanical length |
| LM2 | `Proximal Head` | Proximal endpoint used for biomechanical length |

#### Orientation and biomechanical length

Biomechanical length is the projected distance between LM1 and LM2 along the longitudinal axis. The mediolateral direction is derived from the scanner X axis rather than an anatomical transverse landmark pair.

This mode is available for the Avizo/Amira workflow and is intentionally not supported for 3D Slicer output because its orientation depends on standardized scanner/table positioning.

## Additional long-bone tools

### Whole-bone reorientation with `Reorient()`

`Reorient()` generates software code that places a completed long-bone result in a standardized Cartesian frame.

Z is always longitudinal from distal to proximal and the distal biomechanical endpoint becomes the origin. For `HUMERUS` and `ULNA`, Y is anatomically anterior because AP sign is resolved from landmarks. For `TIBIA`, `FEMUR`, and `RADIUS`, Y follows the acquisition- or coordinate-dependent AP-oriented axis and should not be interpreted as independently landmark-resolved anatomical anterior.

In 3D Slicer mesh-input workflows, `Reorient()` creates a new `_Anatomical` model with transformed vertex coordinates while leaving the source model unchanged.

CT support is intentionally non-resampling. In Slicer, `Reorient()` applies a linear transform and aligns the standard slice viewers but does not resample the voxel lattice. In Avizo/Amira it applies only `setTransform`. Creating an intrinsically reoriented CT/DICOM stack requires external resampling.

### Fragmented-mesh ROI with `get_fragmented_roi()`

For fragmented 3D-scanned specimens, `get_fragmented_roi()` copies a Python block for creating an oriented ROI in 3D Slicer.

```r
get_fragmented_roi("W30")
```

By default, the ROI spans 20-80% of the geometric longitudinal reference and adds a 2 mm transverse margin. The limits can be changed:

```r
get_fragmented_roi("W30", limits = c(10, 90))
```

The longitudinal reference is `LongMax`, calculated from the two mesh vertices with the greatest Euclidean distance. It is a geometric approximation of longitudinal direction and should not be interpreted as the anatomical or biomechanical length calculated by `orient_longbone()`.

A `LongMax` markup line can optionally be created with:

```r
get_fragmented_roi("W30", create_la_line = TRUE)
```

### Batch capture with `flash_capture()`

`flash_capture()` generates one batch command block for several long-bone sections. It supports:

- Avizo/Amira + volume (`INPUT = "VOLUME"`, `SLICER = FALSE`);
- 3D Slicer + volume (`INPUT = "VOLUME"`, `SLICER = TRUE`); and
- 3D Slicer + mesh (`INPUT = "MESH"`, `SLICER = TRUE`).

In 3D Slicer, run one reference section first, usually `SECTION_50`, and configure the desired view before using `flash_capture()`:

```r
copy_slicer_py(res, section = "SECTION_50")

flash_capture(
  res,
  output_dir = r"(C:\OrientCSG\captures)",
  sections = c(20, 35, 50, 65, 80)
)
```

In Avizo/Amira, no prior `copy_tcl()` step is required. Flash Capture initializes the first requested section from the current result and then reuses the prepared camera while moving through the requested section levels.

If `file_name` is omitted, `res$individual_id` is used. RGB is the default capture mode. In 3D Slicer, `color_mode = "grayscale"` converts the rendered RGB buffer to a one-channel luminance image and writes a lossless Deflate-compressed TIFF. In Avizo/Amira, requesting grayscale warns and falls back to the native RGB snapshot.

`viewer_id` and `reference_tolerance_mm` are no longer public arguments. Flash Capture uses Avizo/Amira viewer 0 and a fixed 2 mm Slicer reference-section tolerance internally.

## Software output

### 3D Slicer

Long-bone Slicer output is implemented for `TIBIA`, `HUMERUS`, `FEMUR`, `RADIUS`, and `ULNA`.

For `INPUT = "VOLUME"`, the generated Python code works with a scalar volume. If `volume_name` is left empty, the block first tries to use the background volume in the Red slice view and, if this is unavailable, the only scalar volume loaded in the scene. When several scalar volumes are loaded, providing the exact volume name is recommended.

For `INPUT = "MESH"`, the generated code cuts and displays the corresponding model node. If `model_name` is omitted, OrientCSG uses the basename of `mesh_file` when available.

Generated Slicer blocks are stored in `res$slicer_py`:

```r
names(res$slicer_py)
get_slicer_py(res, section = "SECTION_50")
copy_slicer_py(res, section = "SECTION_50")
```

For long-bone workflows, section names follow the requested percentages, such as `"SECTION_35"` and `"SECTION_50"`. Mandibular section names are `"CS1"`, `"CS2"`, and `"CS3"`.

Long-bone Slicer blocks define `restore_view()`. Volume-input blocks additionally define `refresh_orientcsg_scale()` and `restore_3d_camera()`. The 10 mm OrientCSG scale markup is created automatically but starts hidden by default.

Mandibular Slicer blocks orient the Red slice view to the selected section, create ARP and `LM1_Line` verification objects, add the hidden 10 mm scale, configure the 3D verification view, and define `restore_view()` and `refresh_orientcsg_scale()`.

### Avizo/Amira

The generated TCL code refers to existing Amira/Avizo objects by name. Their names must match exactly.

For mandibular workflows, the expected objects are:

- `ARP`: clipping plane used to display the alveolar reference plane;
- `Slice`: slice object used to display the active cross-section;
- `OrthogonalView`: optional clipping plane used as a visual check.

For long-bone workflows, the expected objects are:

- `Slice`: slice object used for the transverse section;
- `ML`: clipping plane used for the mediolateral anatomical plane;
- `AP`: clipping plane used for the anteroposterior anatomical plane.

Generated TCL blocks can be inspected or copied with `get_tcl()` and `copy_tcl()`.

### Viewing distance and zoom

`camera_distance` is a relative visual-framing factor, not a physical camera distance in millimetres.

The default `camera_distance = 1` uses the calibrated standard view. Values below 1 zoom in and values above 1 zoom out.

For long-bone workflows, OrientCSG maps this factor to the native orthographic zoom control of each backend: Avizo/Amira uses a base `CameraHeight` of 100, Slicer volume input uses a base Red-slice vertical field of view of 70 mm, and Slicer 3D views use a base `ParallelScale` of 35, corresponding to approximately 70 mm of visible vertical height.

Mandibular Slicer output retains its established 85 mm base vertical field of view (`ParallelScale = 42.5` in 3D).

## Examples

The README provides a compact overview. Complete executable examples are distributed with the package in `inst/examples/` and should be treated as the main practical reference.

The long-bone example script includes all supported modes, volume- and mesh-input workflows, all four accepted BoneJ text forms, 3D Slicer examples, `flash_capture()`, and `get_fragmented_roi()`.

The mandibular example script shows the mandibular orientation workflow and both software backends.

Locate the installed examples with:

```r
system.file("examples", package = "OrientCSG")
list.files(system.file("examples", package = "OrientCSG"))
```

Open the long-bone example:

```r
example_file <- system.file(
  "examples",
  "longbone_orientation_example.R",
  package = "OrientCSG"
)

file.edit(example_file)
```

Open the mandibular example:

```r
mandible_example <- system.file(
  "examples",
  "mandible_orientation_example.R",
  package = "OrientCSG"
)

file.edit(mandible_example)
```

A minimal volume-input long-bone call can use the local DICOM series directly:

```r
dicom_dir <- r"(D:\path\to\the\DICOM_series)"

res <- orient_longbone(
  mode = "TIBIA",
  INPUT = "VOLUME",
  longitudinal_matrix_str = longitudinal_matrix_str,
  dicom_dir = dicom_dir,
  landmarks_str = landmarks_str,
  section_loc = 50,
  individual_id = "T108_Left",
  camera_distance = 1
)

res$summary
copy_tcl(res, section = "SECTION_50")
```

On Windows, raw strings such as `r"(D:\path\to\folder)"` are recommended for file and directory paths so paths copied directly from File Explorer can be pasted without escaping backslashes.

## Common issues

Most errors or unexpected orientations are caused by one of the following:

- landmark coordinates supplied in the wrong order;
- an incompatible mandibular preservation option (`complete_arch`, `estimate_lm10`, or `lm9_valid`);
- the wrong number of mandibular landmarks;
- incorrectly copied BoneJ Log/eigenvector input;
- incorrect DICOM Image Orientation (Patient) or Image Position (Patient) values, non-consecutive IPP inputs, or metadata from a different stack than the one processed in BoneJ;
- the wrong long-bone mode;
- a mesh that cannot be read by `Rvcg`, is severely incomplete or degenerate, or produces a near-zero signed volume;
- an incorrect coordinate convention for Slicer landmarks;
- a Slicer model or volume name that does not match the generated code;
- missing or differently named Amira/Avizo objects;
- use of `HUMERUS_TABLE` without standardized scan orientation;
- requesting `SLICER = TRUE` with `HUMERUS_TABLE`;
- an incorrect section name passed to an output helper; or
- running Slicer `flash_capture()` before preparing a normal OrientCSG reference section.

## Utility function

OrientCSG exports `dist3()`, a small utility for computing Euclidean distance between two 3D points:

```r
dist3(c(1, 2, 3), c(5, 5, 3))
```

The returned value is expressed in the same linear unit as the input coordinates.

## Development status

OrientCSG is under active methodological development.

Version 1.1.1 includes volume- and mesh-input long-bone workflows, support for tibia, humerus, femur, radius, and ulna, whole-bone reorientation with `Reorient()`, automatic DICOM metadata extraction, 3D Slicer and Avizo/Amira outputs, batch capture with `flash_capture()`, and preliminary fragmented-mesh ROI generation with `get_fragmented_roi()`.

Planned developments include expanded workflows for fragmented long bones and additional preservation scenarios.

## Methodological documentation

This README is intended as a practical guide to installing and running the package. It does not provide a full methodological justification of the geometric operations implemented in OrientCSG.

For correct use of the package, users should refer to the associated methodological publications and protocols, which describe the anatomical logic behind the reference systems, the rationale for each geometric decision, and the precision and error of the method.

Until these publications are available, OrientCSG should be treated as a research tool under active development.

## Funding and support

Development of this package was supported by the FCT R&D research project "ParaFunction" (project reference 2022.07737.PTDC; [https://doi.org/10.54499/2022.07737.PTDC](https://doi.org/10.54499/2022.07737.PTDC)).

For questions, contact `almartan@ucm.es`.

## References

Ruff, C. B. (2002). Long bone articular and diaphyseal structure in old world monkeys and apes. I: Locomotor effects. *American Journal of Physical Anthropology*, *119*(4), 305-342. [https://doi.org/10.1002/ajpa.10117](https://doi.org/10.1002/ajpa.10117)

Toro-Ibacache, V., Ugarte, F., Morales, C., Eyquem, A., Aguilera, J., & Astudillo, W. (2019). Dental malocclusions are not just about small and weak bones: assessing the morphology of the mandible with cross-section analysis and geometric morphometrics. *Clinical Oral Investigations*, *23*(9), 3479-3490. [https://doi.org/10.1007/s00784-018-2766-6](https://doi.org/10.1007/s00784-018-2766-6)
