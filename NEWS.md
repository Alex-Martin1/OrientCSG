# OrientCSG 0.3.1

* Updated mandibular 3D Slicer Python output so that in-plane slice orientation is controlled by the ARP normal projected into each section plane. This forces the ARP to appear horizontal in the captured slice and makes the Red slice view more closely match the anatomical orientation produced by the Amira/Avizo route.
* `X_SCREEN_REFERENCE` is now used only to resolve the left-right sign of the displayed section, not as the primary in-plane orientation vector.

# OrientCSG 0.3.0

* Added the mandibular 3D Slicer backend for CS1, CS2, and CS3 volume workflows.
