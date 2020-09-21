package ray.common

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Utils{


  implicit def vector3DToColor(s: Vector3D): Color = new Color(s.getX toInt, s.getY toInt, s.getZ toInt)

  implicit def colorToVector3D(s: Color): Vector3D = new Vector3D(s.getRed, s.getGreen, s.getBlue)


}
