package ray.common

import java.awt.Color

import com.badlogic.gdx.math.Vector3
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Utils{


  implicit def vector3DToColor(s: Vector3D): Color = new Color(s.getX toInt, s.getY toInt, s.getZ toInt)

  implicit def colorToVector3D(s: Color): Vector3D = new Vector3D(s.getRed, s.getGreen, s.getBlue)

  implicit def vector3DToVector3(s: Vector3D): Vector3 = new Vector3(s.getX.toFloat, s.getY.toFloat, s.getZ.toFloat)

  implicit def vector3ToVector3D(s: Vector3): Vector3D = new Vector3D(s.x, s.y, s.z)


}
