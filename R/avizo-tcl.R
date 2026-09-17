# Internal Avizo TCL helper --------------------------------------------------
#
# Write a three-component vector into an Avizo port that expects one component
# per `setValue` call. This format is retained for compatibility with older
# point-and-two-vector commands, but generated orientation blocks now prefer
# normal-and-point plane definitions for better Amira/Avizo compatibility.
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
# Compute a plane from three protocol points, but emit it to Avizo as
# origin + normal. This is more reliable for clipping-plane objects than
# sending planePoint1/2/3 directly.
#
# In the mandibular workflow this is used for the ARP object:
# LM1, LM2, and LM1_Line. LM1_Line may be estimated or directly landmarked
# depending on the preservation mode.
emit_plane_3points <- function(obj, P1, P2, P3, color = NULL, hide_points = TRUE, digits = 6) {
  V1 <- P2 - P1
  V2 <- P3 - P1
  N <- nrm(cross3(V1, V2))
  
  # Centroid used only to place the visible plane handle near the landmarks.
  P0 <- (P1 + P2 + P3) / 3
  
  out <- c(
    sprintf('"%s" planeDefinition setValue 0', obj),
    sprintf('"%s" origin setCoord 0 %s', obj, fmt_vec(P0, digits)),
    sprintf('"%s" normal setCoord 0 %s', obj, fmt_vec(N, digits))
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
# Emit commands for a plane defined by one point and two vectors. This helper is
# retained for compatibility and debugging, but current mandibular and long-bone
# TCL blocks use normal-and-point plane definitions for better Amira/Avizo
# compatibility.
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
# for Slice and Clipping Plane objects in the generated TCL blocks.
emit_normal_point_plane <- function(obj, P, N, color = NULL, hide_points = TRUE, digits = 6) {
  out <- c(
    sprintf('"%s" planeDefinition setValue 0', obj),
    sprintf('"%s" origin setCoord 0 %s', obj, fmt_vec(P, digits)),
    sprintf('"%s" normal setCoord 0 %s', obj, fmt_vec(N, digits))
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

# Backward-compatible internal alias.
emit_slice_normal_point <- function(obj, P, N, digits = 6) {
  emit_normal_point_plane(obj, P, N, digits = digits)
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit commands that place the Avizo/Amira camera according to a geometric basis
# computed in R. The section is shown parallel to the screen, while `X_axis` and
# `Y_preferred` control the screen-horizontal and screen-vertical anatomical
# orientation. The camera is set to orthographic view by default because that is
# the appropriate projection for systematic image capture.
emit_camera_from_basis <- function(P, Z_axis, X_axis, Y_preferred = NULL,
                                   camera_distance = 1, viewer_id = 0,
                                   orthographic = TRUE, orientation_only = FALSE,
                                   preserve_screen_position = FALSE,
                                   anchor_expr = NULL,
                                   digits = 6) {
  basis <- make_camera_basis(Z_axis = Z_axis, X_axis = X_axis, Y_preferred = Y_preferred)
  Xcam <- basis$Xcam
  Ycam <- basis$Ycam
  Zcam <- basis$Zcam

  R_cam <- cbind(Xcam, Ycam, Zcam)
  aa <- rotmat_to_axis_angle(R_cam)
  ax <- aa$axis
  ang <- aa$angle

  orientation_cmd <- sprintf(
    "viewer %d setCameraOrientation %s %s %s %s",
    viewer_id,
    fmt_num(ax[1], digits), fmt_num(ax[2], digits), fmt_num(ax[3], digits),
    fmt_num(ang, digits)
  )

  # Flash Capture can preserve the visual location of the reference section
  # while rotating to a new anatomical basis. The current camera position and
  # orientation are queried at runtime. The reference point keeps the same X/Y
  # screen coordinates and depth in the new camera basis. Camera height/zoom,
  # projection type, focal distance, and clipping distances are not touched.
  if (isTRUE(preserve_screen_position)) {
    Dcam <- -Zcam
    pref_cmd <- if (is.null(anchor_expr)) {
      sprintf("set OrientCSG_Pref {%s}", fmt_vec(P, digits))
    } else {
      sprintf("set OrientCSG_Pref %s", anchor_expr)
    }
    return(c(
      pref_cmd,
      sprintf("set OrientCSG_Xnew {%s}", fmt_vec(Xcam, digits)),
      sprintf("set OrientCSG_Ynew {%s}", fmt_vec(Ycam, digits)),
      sprintf("set OrientCSG_Dnew {%s}", fmt_vec(Dcam, digits)),
      "proc _orientcsg_dot {a b} {expr {[lindex $a 0]*[lindex $b 0] + [lindex $a 1]*[lindex $b 1] + [lindex $a 2]*[lindex $b 2]}}",
      "proc _orientcsg_cross {a b} {list [expr {[lindex $a 1]*[lindex $b 2] - [lindex $a 2]*[lindex $b 1]}] [expr {[lindex $a 2]*[lindex $b 0] - [lindex $a 0]*[lindex $b 2]}] [expr {[lindex $a 0]*[lindex $b 1] - [lindex $a 1]*[lindex $b 0]}]}",
      "proc _orientcsg_scale {v s} {list [expr {[lindex $v 0]*$s}] [expr {[lindex $v 1]*$s}] [expr {[lindex $v 2]*$s}]}",
      "proc _orientcsg_add {a b} {list [expr {[lindex $a 0]+[lindex $b 0]}] [expr {[lindex $a 1]+[lindex $b 1]}] [expr {[lindex $a 2]+[lindex $b 2]}]}",
      "proc _orientcsg_sub {a b} {list [expr {[lindex $a 0]-[lindex $b 0]}] [expr {[lindex $a 1]-[lindex $b 1]}] [expr {[lindex $a 2]-[lindex $b 2]}]}",
      "proc _orientcsg_norm {v} {set n [expr {sqrt([_orientcsg_dot $v $v])}]; if {$n < 1e-12} {return {0 0 1}}; list [expr {[lindex $v 0]/$n}] [expr {[lindex $v 1]/$n}] [expr {[lindex $v 2]/$n}]}",
      "proc _orientcsg_rotate_axis_angle {v axis angle} {set u [_orientcsg_norm $axis]; set c [expr {cos($angle)}]; set s [expr {sin($angle)}]; set t1 [_orientcsg_scale $v $c]; set t2 [_orientcsg_scale [_orientcsg_cross $u $v] $s]; set t3 [_orientcsg_scale $u [expr {[_orientcsg_dot $u $v] * (1.0 - $c)}]]; _orientcsg_add [_orientcsg_add $t1 $t2] $t3}",
      sprintf("set OrientCSG_camPos [viewer %d getCameraPosition]", viewer_id),
      sprintf("set OrientCSG_camOri [viewer %d getCameraOrientation]", viewer_id),
      "set OrientCSG_axis [lrange $OrientCSG_camOri 0 2]",
      "set OrientCSG_ang [lindex $OrientCSG_camOri 3]",
      "set OrientCSG_Xold [_orientcsg_rotate_axis_angle {1 0 0} $OrientCSG_axis $OrientCSG_ang]",
      "set OrientCSG_Yold [_orientcsg_rotate_axis_angle {0 1 0} $OrientCSG_axis $OrientCSG_ang]",
      "set OrientCSG_Dold [_orientcsg_rotate_axis_angle {0 0 -1} $OrientCSG_axis $OrientCSG_ang]",
      "set OrientCSG_dold [_orientcsg_sub $OrientCSG_Pref $OrientCSG_camPos]",
      "set OrientCSG_keepX [_orientcsg_dot $OrientCSG_dold $OrientCSG_Xold]",
      "set OrientCSG_keepY [_orientcsg_dot $OrientCSG_dold $OrientCSG_Yold]",
      "set OrientCSG_keepD [_orientcsg_dot $OrientCSG_dold $OrientCSG_Dold]",
      "set OrientCSG_offset [_orientcsg_add [_orientcsg_add [_orientcsg_scale $OrientCSG_Xnew $OrientCSG_keepX] [_orientcsg_scale $OrientCSG_Ynew $OrientCSG_keepY]] [_orientcsg_scale $OrientCSG_Dnew $OrientCSG_keepD]]",
      "set OrientCSG_Cnew [_orientcsg_sub $OrientCSG_Pref $OrientCSG_offset]",
      sprintf("viewer %d setCameraPosition [lindex $OrientCSG_Cnew 0] [lindex $OrientCSG_Cnew 1] [lindex $OrientCSG_Cnew 2]", viewer_id),
      orientation_cmd,
      sprintf("viewer %d redraw", viewer_id)
    ))
  }

  # Flash Capture can request orientation-only output so that the current
  # camera position, projection type, and zoom/framing are preserved while
  # the camera is rotated perpendicular to the current section.
  if (isTRUE(orientation_only)) {
    return(c(orientation_cmd, sprintf("viewer %d redraw", viewer_id)))
  }

  # In orthographic projection, moving the camera does not change visual zoom.
  # Keep a stable internal camera position and map the public camera_distance
  # factor to the orthographic view height instead.
  camera_position_distance_mm <- 300
  base_camera_height <- 100
  camera_height <- base_camera_height * camera_distance
  C <- P + camera_position_distance_mm * Zcam

  out <- c(
    sprintf("set Cx %s", fmt_num(C[1], digits)),
    sprintf("set Cy %s", fmt_num(C[2], digits)),
    sprintf("set Cz %s", fmt_num(C[3], digits)),
    sprintf("viewer %d setCameraPosition $Cx $Cy $Cz", viewer_id),
    orientation_cmd
  )

  if (orthographic) {
    out <- c(
      out,
      sprintf("viewer %d setCameraType orthographic", viewer_id),
      sprintf("viewer %d setCameraHeight %s", viewer_id, fmt_num(camera_height, digits))
    )
  }

  c(
    out,
    sprintf('catch {viewer %d setCameraFocalDistance %s}', viewer_id, fmt_num(camera_position_distance_mm, digits)),
    sprintf('catch {viewer %d setCameraNearDistance 1}', viewer_id),
    sprintf('catch {viewer %d setCameraFarDistance %s}', viewer_id, fmt_num(camera_position_distance_mm * 4, digits)),
    sprintf("viewer %d redraw", viewer_id)
  )
}

# Internal Avizo TCL helper --------------------------------------------------
#
# Emit optional commands for an object named OrthogonalView. When this object is
# present in the Avizo project, it is used as a visual check plane perpendicular
# to the screen. The caller supplies the plane normal, usually the section
# direction shown horizontally on screen. The commands are wrapped in `catch`,
# so the generated TCL block still works when OrthogonalView has not been created.
emit_optional_orthogonal_view <- function(P, N, label, digits = 6) {
  c(
    sprintf('# Optional visual check plane for %s; ignored if object "OrthogonalView" does not exist.', label),
    sprintf('catch {"OrthogonalView" planeDefinition setValue 0}'),
    sprintf('catch {"OrthogonalView" origin setCoord 0 %s}', fmt_vec(P, digits)),
    sprintf('catch {"OrthogonalView" normal setCoord 0 %s}', fmt_vec(N, digits)),
    sprintf('catch {"OrthogonalView" fire}')
  )
}
