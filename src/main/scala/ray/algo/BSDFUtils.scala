package ray.algo

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import org.apache.log4j.{LogManager, Logger}
import ray.common.MyRay.RayIntersection
import ray.common.{MaterialEta, Object3D}


object BSDFUtils{
  lazy val logger: Logger = LogManager.getLogger(getClass.getName.stripSuffix("$"))

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

  def refract(iz: Vector3D, n: Vector3D): Vector3D = {
    val (i, η, z, cos) = iz dotProduct n match {
      case v if FastMath.abs(v) - 1 < 0.000001 => return iz
      case v if v <= 0 =>

        /**
          * 从空气进入介质
          */
        (iz, MaterialEta.AIR.eta / MaterialEta.GLASS.eta, 1, v)
      case v if v > 0 => (iz negate, MaterialEta.GLASS.eta / MaterialEta.AIR.eta, 0, v)
    }

    val t1 = FastMath.sqrt(1 - η * η * (1 - FastMath.pow(n dotProduct i, 2)))
    val t0 = n scalarMultiply (η * (n dotProduct i) - t1) subtract (i scalarMultiply η)

    logger.info(s"theta: $cos t1: $t1 t0: $t0 debug: ${(1 - FastMath.pow(n dotProduct i, 2))} z: $z")
    logger.info(s"iz: $iz n: $n")
    t0
  }
}
