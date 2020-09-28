package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.PathTracing.eye

object Ray{
  def intersect(o: Vector3D, dir: Vector3D, objs: Set[Object3D],
                diffObj: Set[Object3D] = Set.empty) = for {
    obj <- objs diff diffObj
    intersection = obj.intersect(o, dir)
    if intersection._1
  } yield RayIntersection(dir, intersection._2, obj)

  case class RayIntersection(originDir: Vector3D, distance: Double, obj: Object3D)

  @deprecated
  def rayIntersections(o: Vector3D, dir: Vector3D, objs: Set[Object3D]) = {
    val r = for {
      obj <- objs

      intersection = obj.intersect(eye, dir)

      if intersection._1
    } yield {
      RayIntersection(dir, intersection._2, obj)
    }

    r.toArray
  }

  def seekNearestObj(intersectedObjs: Array[RayIntersection]) = {
    intersectedObjs.reduce {
      (a, b) =>
        if (a.distance < b.distance) {
          a
        } else {
          b
        }
    }
  }
}
