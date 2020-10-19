package ray.algo

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MyRay.RayIntersection
import ray.common.{MaterialEta, Object3D}


object BSDFUtils{

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): RayIntersection = {
    RayIntersection(s.dir, s.d, s.obj)
  }


  def allCollision(o: Vector3D, dir: Vector3D, objs: Set[Object3D]) = {
    for {
      obj <- objs
      intersection = obj.intersect(o, dir)
      if intersection._1
    } yield {
      RayIntersection(dir, intersection._2, obj)
    }
  }


  def computeEtaPair(hitDir: Vector3D, norm: Vector3D) = {
    val (eta1, eta2) = hitDir dotProduct norm match {
      case v if v >= 0 =>
        (MaterialEta.AIR.eta, MaterialEta.GLASS.eta)
      case _ =>
        (MaterialEta.GLASS.eta, MaterialEta.AIR.eta)
    }
    (eta1, eta2)
  }


  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)

  def reflect(dir: Vector3D, n: Vector3D): Vector3D = {
    dir.subtract(n scalarMultiply (2 * (n dotProduct dir)))
  }

  def refract(dir: Vector3D, n: Vector3D, eta1: Double, eta2: Double): Vector3D = {
    val tt = (dir add (n scalarMultiply (n dotProduct dir))) scalarMultiply (eta1 / eta2)
    val tt1 = 1 - (eta1 * eta1) / (eta2 * eta2) * (1 - Math.pow(dir dotProduct n, 2))
    val tt2 = n scalarMultiply Math.sqrt(tt1)
    val refractDir = (tt subtract tt2) normalize

    refractDir
  }
}
