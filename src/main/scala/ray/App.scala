package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.Material.Material
import ray.common.{Material, Object3D, Surface}
import ray.scenes.PlaneTest


object App{
  val scene = PlaneTest
  private val xi = 1900
  private val yi = 1500


  val width = 0 until xi toArray
  val height = 0 until yi toArray


  val eye = new Vector3D(xi / 2, yi / 2, -900f)
  val light = scene.light


  def main(args: Array[String]): Unit = {
    rayTrace
  }

  private def rayTrace = {
    val pixs = for {
      x <- width
      y <- height
    } yield {
      (x, y)
    }


    val newBufferedImage = new BufferedImage(width.size, height.size, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until xi
      y <- 0 until yi
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    renderScene(pixs, eye, scene.scene).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.png")
    ImageIO.write(newBufferedImage, "PNG", file)
  }

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): IntersectStatus = {
    IntersectStatus(s.dir, s.d, s.obj)
  }

  private def renderScene(pixs: Array[(Int, Int)], eye: Vector3D, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

    val pixToEyePathAllIntersectedObjs = rayIntersections(pixs, eye, objs)
      .groupBy(x => x.pix)

    val pixColor = for {
      rayIntersection <- pixToEyePathAllIntersectedObjs
    } yield {
      val statuses = rayIntersection._2.map(x => {
        val r: IntersectStatus = x
        r
      }

      )
      (rayIntersection._1._1, rayIntersection._1._2, trace(eye, objs, statuses, 8, Material.AIR))
    }

    pixColor toArray

  }

  private def trace(eye: Vector3D, objs: Set[Object3D], intersectedObjs: Array[IntersectStatus], depth: Int,
                    curMaterial: Material = Material.AIR): Color = {
    if (depth == 0 || intersectedObjs.size == 0) {
      return Color.GRAY
    }
    val nearestObj = seekNearestObj(intersectedObjs)
    val otherObjs = objs diff Set(nearestObj.obj)

    val phit: Vector3D = eye add (nearestObj.originDir scalarMultiply nearestObj.distance)
    val phitToLight = light.center subtract phit
    val maxDistance = light.center.distance(phit)
    val hitToLightDir: Vector3D = phitToLight normalize()
    val notIntersectedObjs = otherObjs.takeWhile { object3D =>
      val r = object3D.intersect(phit, hitToLightDir)
      r._1 == false || r._2 > maxDistance
    }

    val c = otherObjs.size - notIntersectedObjs.size match {
      case 0 =>
        trace$(eye, objs, depth, nearestObj, phit, .4, .6, curMaterial)
      case _ =>
        trace$(eye, objs, depth, nearestObj, phit, .01, .01, curMaterial)
    }

    c
  }

  private def trace$(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: IntersectStatus, phit: Vector3D,
                     amb: Double, spec: Double, curMaterial: Material = Material.AIR): Color = {
    lazy val hitNorm = nearestIntersection.obj.normal(phit)

    nearestIntersection.obj.surface match {
      case Surface.REFLECTIVE =>
        computeReflection(objs, depth, nearestIntersection, phit, hitNorm)
      case Surface.REGULAR => Phong.renderPix(eye, nearestIntersection.originDir, nearestIntersection.distance,
        light, nearestIntersection.obj, amb, spec, nearestIntersection.obj.color)
      case Surface.REFRACTIVE =>
        import ray.common.Utils._

        val refl: Vector3D = computeReflection(objs, depth, nearestIntersection, phit, hitNorm)

        val viewDir = (phit subtract eye) normalize
        val (eta1: Double, eta2: Double) = computeEtaPair(viewDir, hitNorm)

        val rdir = refractionDir(phit subtract eye, hitNorm, eta1, eta2)

        val costheta1 = viewDir dotProduct hitNorm
        val costheta2 = rdir dotProduct hitNorm

        val fr1 = (eta2 * costheta1 - eta1 * costheta2) / (eta2 * costheta1 + eta1 * costheta2)
        val fr2 = (eta1 * costheta2 - eta2 * costheta1) / (eta1 * costheta2 + eta2 * costheta1)


        val sinTheta2 = Math.sqrt(1 - costheta2 * costheta2)

        val kr = sinTheta2 match {
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

  private def computeReflection(objs: Set[Object3D], depth: Int, nearestIntersection: IntersectStatus, phit: Vector3D,
                                n: Vector3D): Color = {
    import ray.common.Utils._
    val reflect = nearestIntersection.originDir.subtract(n scalarMultiply (2 * (n dotProduct nearestIntersection.originDir)))

    val intersectStatuses = for {
      obj <- objs diff Set(nearestIntersection.obj)
      intersection = obj.intersect(phit, reflect)
      if intersection._1
    } yield IntersectStatus(reflect, intersection._2, obj)

    trace(phit, objs, intersectStatuses.toArray, depth - 1) scalarMultiply .85
  }

  private def computeRefraction(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: IntersectStatus,
                                phit: Vector3D, n: Vector3D): Color = {
    val eyeToPhit = phit subtract eye
    val viewDir = eyeToPhit.normalize()

    val (eta1: Double, eta2: Double) = computeEtaPair(viewDir, nearestIntersection.obj.normal(phit))

    val refractDir = refractionDir(eyeToPhit, n, eta1, eta2)

    val bias = nearestIntersection.originDir scalarMultiply 16

    val phit1 = phit add bias
    val intersectStatuses = for {
      obj <- objs
      intersection = obj.intersect(phit1, refractDir)
      if intersection._1
    } yield IntersectStatus(refractDir, intersection._2, obj)


    trace(phit, objs, intersectStatuses.toArray, depth - 1, nearestIntersection.obj.material)
  }

  private def refractionDir(eyeToPhit: Vector3D, n: Vector3D, eta1: Double, eta2: Double): Vector3D = {
    val tt = (eyeToPhit add (n scalarMultiply (n dotProduct eyeToPhit))) scalarMultiply (eta1 / eta2)
    val tt1 = 1 - (eta1 * eta1) / (eta2 * eta2) * (1 - Math.pow(eyeToPhit dotProduct n, 2))
    val tt2 = n scalarMultiply Math.sqrt(tt1)
    val refractDir = (tt subtract tt2) normalize

    refractDir
  }

  private def computeEtaPair(hitDir: Vector3D, norm: Vector3D) = {
    val (eta1, eta2) = hitDir dotProduct norm match {
      case v if v >= 0 && v <= 1 =>
        //inside object
        (Material.AIR.eta, Material.GLASS.eta)
      case _ =>
        (Material.GLASS.eta, Material.AIR.eta)
    }
    (eta1, eta2)
  }

  private def seekNearestObj(intersectedObjs: Array[IntersectStatus]) = {
    intersectedObjs.reduce {
      (a, b) =>
        if (a.distance < b.distance) {
          a
        } else {
          b
        }
    }
  }

  private def rayIntersections(pixs: Array[(Int, Int)], eye: Vector3D, objs: Set[Object3D]) = {
    for {
      pix <- pixs
      obj <- objs

      eyeToPix = computeRay(eye, pix)
      intersection = obj.intersect(eye, eyeToPix)

      if intersection._1
    } yield PixIntersectedObj(pix, eyeToPix, intersection._2, obj)
  }

  private def computeRay(eye: Vector3D, xy: (Int, Int)) = {
    val pix = new Vector3D(xy._1, xy._2, 0)
    (pix subtract eye) normalize
  }

  case class IntersectStatus(originDir: Vector3D, distance: Double, obj: Object3D)

  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)


}
