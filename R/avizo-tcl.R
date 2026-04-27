# Internal Avizo TCL helper --------------------------------------------------
#
# Write a three-component vector into an Avizo port that expects one component
# per `setValue` call. This format is used by several plane objects when their
# orientation is defined by two in-plane vectors.
emit_setValue_vec3 <- function(obj, port, v, digits = 6) {
  c(
    sprintf('"%s" %s setValue 0 %s', obj, port, fmt_num(v[1], digits)),
    sprintf('"%s" %s setValue 1 %s', obj, port, fmt_num(v[2], digits)),
    sprintf('"%s" %s setValue 2 %s', obj, port, fmt_num(v[3], digits))
  )
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Hide auxiliary control points that may be displayed by Slice or Clipping Plane
# objects after they are redefined through the command line. These commands are
# wrapped in `catch` when emitted because not every Avizo/Amira version exposes
# exactly the same point ports for every object.
emit_hide_plane_points <- function(obj) {
  c(
    sprintf('catch {"%s" origin showPoints 0}', obj),
    sprintf('catch {"%s" point showPoints 0}', obj),
    sprintf('catch {"%s" planePoint1 showPoints 0}', obj),
    sprintf('catch {"%s" planePoint2 showPoints 0}', obj),
    sprintf('catch {"%s" planePoint3 showPoints 0}', obj)
  )
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit commands for a plane defined by three points. In the mandibular workflow
# this is used to orient the ARP object from LM1, LM2, and LM1_Line. The function
# assumes that the target object already exists in Avizo/Amira and has the name
# supplied in `obj`.
emit_plane_3points <- function(obj, P1, P2, P3, color = NULL, hide_points = TRUE, digits = 6) {
  out <- c(
    sprintf('"%s" planeDefinition setValue 1', obj),
    sprintf('"%s" planePoint1 setCoord 0 %s', obj, fmt_vec(P1, digits)),
    sprintf('"%s" planePoint2 setCoord 0 %s', obj, fmt_vec(P2, digits)),
    sprintf('"%s" planePoint3 setCoord 0 %s', obj, fmt_vec(P3, digits))
  )

  if (hide_points) out <- c(out, emit_hide_plane_points(obj))

  if (!is.null(color) && length(color) == 3) {
    out <- c(
      out,
      sprintf(
        'catch {"%s" frameSettings setState item 0 1 item 2 1 color 3 %s %s %s 0}',
        obj, fmt_num(color[1], 3), fmt_num(color[2], 3), fmt_num(color[3], 3)
      )
    )
  }

  c(out, sprintf('"%s" fire', obj))
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit commands for a plane defined by one point and two vectors. This is used
# for mandibular CS1 and CS2, where one vector follows the landmark-defined
# cross-sectional direction and the other enforces perpendicularity to the ARP.
# It is also used to display the long-bone ML and AP anatomical planes.
emit_point_2vectors_plane <- function(obj, P, V1, V2, color = NULL, hide_points = TRUE, digits = 6) {
  P2 <- P + V1
  P3 <- P + V2

  out <- c(
    sprintf('"%s" planeDefinition setValue 2', obj),
    sprintf('"%s" planePoint1 setCoord 0 %s', obj, fmt_vec(P, digits)),
    sprintf('"%s" planePoint2 setCoord 0 %s', obj, fmt_vec(P2, digits)),
    sprintf('"%s" planePoint3 setCoord 0 %s', obj, fmt_vec(P3, digits)),
    emit_setValue_vec3(obj, "planeVector1", V1, digits),
    emit_setValue_vec3(obj, "planeVector2", V2, digits)
  )

  if (hide_points) out <- c(out, emit_hide_plane_points(obj))

  if (!is.null(color) && length(color) == 3) {
    out <- c(
      out,
      sprintf(
        'catch {"%s" frameSettings setState item 0 1 item 2 1 color 3 %s %s %s 0}',
        obj, fmt_num(color[1], 3), fmt_num(color[2], 3), fmt_num(color[3], 3)
      )
    )
  }

  c(out, sprintf('"%s" fire', obj))
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit commands for a plane defined by a point and a normal vector. This is used
# for long-bone transverse sections and for mandibular CS3.
emit_slice_normal_point <- function(obj, P, N, digits = 6) {
  c(
    sprintf('"%s" planeDefinition setValue 0', obj),
    sprintf('"%s" origin setCoord 0 %s', obj, fmt_vec(P, digits)),
    sprintf('"%s" normal setCoord 0 %s', obj, fmt_vec(N, digits)),
    emit_hide_plane_points(obj),
    sprintf('"%s" fire', obj)
  )
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit commands that place the Avizo/Amira camera according to a geometric basis
# computed in R. The section is shown parallel to the screen, while `X_axis` and
# `Y_preferred` control the screen-horizontal and screen-vertical anatomical
# orientation. The camera is set to orthographic view by default because that is
# the appropriate projection for systematic image capture.
emit_camera_from_basis <- function(P, Z_axis, X_axis, Y_preferred = NULL,
                                   camDist = 300, viewer_id = 0,
                                   orthographic = TRUE, digits = 6) {
  basis <- make_camera_basis(Z_axis = Z_axis, X_axis = X_axis, Y_preferred = Y_preferred)
  Xcam <- basis$Xcam
  Ycam <- basis$Ycam
  Zcam <- basis$Zcam

  C <- P + camDist * Zcam
  R_cam <- cbind(Xcam, Ycam, Zcam)
  aa <- rotmat_to_axis_angle(R_cam)
  ax <- aa$axis
  ang <- aa$angle

  out <- c(
    sprintf("set Cx %s", fmt_num(C[1], digits)),
    sprintf("set Cy %s", fmt_num(C[2], digits)),
    sprintf("set Cz %s", fmt_num(C[3], digits)),
    sprintf("viewer %d setCameraPosition $Cx $Cy $Cz", viewer_id),
    sprintf(
      "viewer %d setCameraOrientation %s %s %s %s",
      viewer_id,
      fmt_num(ax[1], digits), fmt_num(ax[2], digits), fmt_num(ax[3], digits),
      fmt_num(ang, digits)
    )
  )

  if (orthographic) {
    out <- c(out, sprintf("viewer %d setCameraType orthographic", viewer_id))
  }

  c(
    out,
    sprintf('catch {viewer %d setCameraFocalDistance %s}', viewer_id, fmt_num(camDist, digits)),
    sprintf('catch {viewer %d setCameraNearDistance 1}', viewer_id),
    sprintf('catch {viewer %d setCameraFarDistance %s}', viewer_id, fmt_num(camDist * 4, digits)),
    sprintf("viewer %d redraw", viewer_id)
  )
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit optional commands for an object named OrthogonalView. When this object is
# present in the Avizo/Amira project, it can be used as a visual check of the
# camera direction. The commands are wrapped in `catch`, so the generated TCL
# block still works when OrthogonalView has not been created.
emit_optional_orthogonal_view <- function(P, N, label, digits = 6) {
  c(
    sprintf('# Optional visual check plane for %s; ignored if object "OrthogonalView" does not exist.', label),
    sprintf('catch {"OrthogonalView" planeDefinition setValue 0}'),
    sprintf('catch {"OrthogonalView" origin setCoord 0 %s}', fmt_vec(P, digits)),
    sprintf('catch {"OrthogonalView" normal setCoord 0 %s}', fmt_vec(N, digits)),
    sprintf('catch {"OrthogonalView" fire}')
  )
}
