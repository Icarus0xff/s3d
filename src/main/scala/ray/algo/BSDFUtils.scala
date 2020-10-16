package ray.algo

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MyRay.RayIntersection
import ray.common.{MaterialEta, MyRay, Object3D, Surface}


object BSDFUtils{

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): RayIntersection = {
    RayIntersection(s.dir, s.d, s.obj)
  }

  def traceRay(o: Vector3D, dir: Vector3D, objs: Set[Object3D]): Color = {
    val oobbjjs = allCollision(o, dir, objs)
    if (oobbjjs.size == 0) {
      Color.GRAY
    }

    val array = oobbjjs.toArray
    trace(o, objs, array, 8, MyRay.seekNearestObj(array))
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

  def trace(o: Vector3D, objs: Set[Object3D], intersectedObjs: Array[RayIntersection], depth: Int, nearestObj: RayIntersection): Color = {
    if (depth == 0) {
      return Color.GRAY
    }

    val phit = o add (nearestObj.originDir scalarMultiply nearestObj.distance)

    trace$(o, objs, depth, nearestObj, phit, .4, .6)
  }

  private def trace$(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: RayIntersection,
                     phit: Vector3D, amb: Double, spec: Double): Color = {
    lazy val hitNorm = nearestIntersection.obj.normal(phit)

    nearestIntersection.obj.surface match {
      case Surface.REFLECTIVE =>
        computeReflection(objs, depth, nearestIntersection, phit, hitNorm)
      case Surface.REGULAR => Color.BLUE
      case Surface.REFRACTIVE =>
        import ray.common.Utils._
        val refl: Vector3D = computeReflection(objs, depth, nearestIntersection, phit, hitNorm)


        val viewDir = (phit subtract eye) normalize


        val (eta1: Double, eta2: Double) = computeEtaPair(viewDir, hitNorm)

        val rdir = refractionDir(phit subtract eye, hitNorm, eta1, eta2)

        val cosInTheta1 = viewDir dotProduct hitNorm
        val cosOutTheta2 = rdir dotProduct hitNorm

        val fr1 = (eta2 * cosInTheta1 - eta1 * cosOutTheta2) / (eta2 * cosInTheta1 + eta1 * cosOutTheta2)
        val fr2 = (eta1 * cosOutTheta2 - eta2 * cosInTheta1) / (eta1 * cosOutTheta2 + eta2 * cosInTheta1)


        val sinOutTheta = Math.sqrt(1 - cosOutTheta2 * cosOutTheta2)

        val kr = sinOutTheta match {
          case v if v > 0.999 => 1
          case _ => 0.5 * (fr1 * fr1 + fr2 * fr2)
        }


        val refr: Vector3D = kr match {
          case krr if krr < 1 => computeRefraction(eye, objs, depth, nearestIntersection, phit, hitNorm)
          case _ =>
            Vector3D.ZERO
        }

        val r = refl scalarMultiply kr add (refr scalarMultiply (1 - kr))
        r

      case Surface.LIGHT => Color.WHITE

    }
  }

  private def computeReflection(objs: Set[Object3D], depth: Int, nearestIntersection: RayIntersection, phit: Vector3D,
                                n: Vector3D): Color = {
    import ray.common.Utils._
    val reflect = nearestIntersection.originDir.subtract(n scalarMultiply (2 * (n dotProduct nearestIntersection.originDir)))

    val rayIntersections = MyRay.intersect(phit, reflect, objs, Set(nearestIntersection.obj))

    val ris = rayIntersections.toArray
    val nobj = MyRay.seekNearestObj(ris)
    trace(phit, objs, ris, depth - 1, nobj) scalarMultiply .75
  }

  private def computeRefraction(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: RayIntersection,
                                phit: Vector3D, n: Vector3D): Color = {
    val eyeToPhit = phit subtract eye
    val viewDir = eyeToPhit.normalize()

    val (eta1: Double, eta2: Double) = computeEtaPair(viewDir, nearestIntersection.obj.normal(phit))

    val refractDir = refractionDir(eyeToPhit, n, eta1, eta2)

    val bias = nearestIntersection.originDir scalarMultiply 16

    val phit1 = phit add bias
    val rayIntersections = for {
      obj <- objs
      intersection = obj.intersect(phit1, refractDir)
      if intersection._1
    } yield RayIntersection(refractDir, intersection._2, obj)


    val iobjs = rayIntersections.toArray
    trace(phit, objs, iobjs, depth - 1, MyRay.seekNearestObj(iobjs))
  }

  private def refractionDir(eyeToPhit: Vector3D, n: Vector3D, eta1: Double, eta2: Double): Vector3D = {
    val tt = (eyeToPhit add (n scalarMultiply (n dotProduct eyeToPhit))) scalarMultiply (eta1 / eta2)
    val tt1 = 1 - (eta1 * eta1) / (eta2 * eta2) * (1 - Math.pow(eyeToPhit dotProduct n, 2))
    val tt2 = n scalarMultiply Math.sqrt(tt1)
    val refractDir = (tt subtract tt2) normalize

    refractDir
  }

  def computeEtaPair(hitDir: Vector3D, norm: Vector3D) = {
    val (eta1, eta2) = hitDir dotProduct norm match {
      case v if v >= 0 && v <= 1 =>
        //inside object
        (MaterialEta.AIR.eta, MaterialEta.GLASS.eta)
      case _ =>
        (MaterialEta.GLASS.eta, MaterialEta.AIR.eta)
    }
    (eta1, eta2)
  }

  private def computeRay(eye: Vector3D, xy: (Int, Int)) = {
    val pix = new Vector3D(xy._1, xy._2, 0)
    (pix subtract eye) normalize
  }


  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)


}
