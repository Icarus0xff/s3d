package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MaterialEta.MaterialEta
import ray.common.Surface.Surface



object Surface extends Enumeration{
  type Surface = Value
  val REGULAR, REFLECTIVE, REFRACTIVE, LIGHT = Value
}


trait Object3D{
  val color: Vector3D
  val surface: Surface
  val materialEta: MaterialEta

  def normal(p: Vector3D): Vector3D

  def intersect(o: Vector3D, dir: Vector3D): (Boolean, Double)

  def surfaceArea(): Double

  def uniformSample(): Vector3D

}
