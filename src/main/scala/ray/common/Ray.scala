package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Ray{
  def intersect(phit: Vector3D, dir: Vector3D, objs: Set[Object3D],
                diffObj: Object3D) = for {
    obj <- objs diff Set(diffObj)
    intersection = obj.intersect(phit, dir)
    if intersection._1
  } yield RayIntersection(dir, intersection._2, obj)

  case class RayIntersection(originDir: Vector3D, distance: Double, obj: Object3D)

}
