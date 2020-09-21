package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Surface.Surface
import ray.common.Utils.Vec3f


object Surface extends Enumeration{
  type Surface = Value
  val REGULAR, REFLECTIVE, REFRACTIVE, LIGHT = Value
}


trait Object3D{
  val color: Vector3D
  val surface: Surface

  val material = Material.NONE

  def normal(p: Vec3f): Vec3f

  def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double)

}
