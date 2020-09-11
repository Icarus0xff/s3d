package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils.Vec3f

trait Object3D{
  val color: Vector3D

  def normal(p: Vec3f): Vec3f

  def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double)
}
