package ray.algo

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MaterialEta.MaterialEta
import ray.common.MyRay.RayIntersection
import ray.common.{MaterialEta, Object3D, MyRay, Surface}
import ray.scenes.LightDraw


object RayTracing{
  val scene = LightDraw
  private val xi = 1900
  private val yi = 1500


  val width = 0 until xi toArray
  val height = 0 until yi toArray


  val eye = new Vector3D(xi / 2, yi / 2, -700f)
  val light = scene.light


  def outputRenderedPic = {
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

    renderScene(pixs, eye, scene.sceneObjs).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.png")
    ImageIO.write(newBufferedImage, "PNG", file)
  }

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): RayIntersection = {
    RayIntersection(s.dir, s.d, s.obj)
  }

  private def renderScene(pixs: Array[(Int, Int)], eye: Vector3D, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

    val pixToEyePathAllIntersectedObjs = rayIntersections(pixs, eye, objs)
      .groupBy(x => x.pix)

    val pixColor = for {
      rayIntersection <- pixToEyePathAllIntersectedObjs
    } yield {
      val rayIntersections = rayIntersection._2.map(x => {
        val r: RayIntersection = x
        r
      }

      )
      (rayIntersection._1._1, rayIntersection._1._2, trace(eye, objs, rayIntersections, 8, MyRay.seekNearestObj(rayIntersections), rayIntersections.size == 0, MaterialEta.AIR))
    }

    pixColor toArray

  }

  private def trace(eye: Vector3D, objs: Set[Object3D], intersectedObjs: Array[RayIntersection], depth: Int, nearestObj: RayIntersection, isNoIntersect: Boolean, curMaterial: MaterialEta = MaterialEta.AIR): Color = {
    if (depth == 0 || isNoIntersect) {
      return Color.GRAY
    }

    val otherObjs = objs diff Set(nearestObj.obj)

    val phit: Vector3D = eye add (nearestObj.originDir scalarMultiply nearestObj.distance)
    val phitToLight = light.center subtract phit
    val maxDistance = light.center.distance(phit)
    val phitToLightDir: Vector3D = phitToLight normalize()
    val notIntersectedObjs = otherObjs.takeWhile { object3D =>
      val r = object3D.intersect(phit, phitToLightDir)
      r._1 == false || r._2 > maxDistance
    }


    val c = otherObjs.size - notIntersectedObjs.size match {
      case 0 =>
        trace$(eye, objs, depth, nearestObj, phit, .4, .6, curMaterial)
      case _ =>
        // in shadow
        trace$(eye, objs, depth, nearestObj, phit, .01, .01, curMaterial)
    }

    c
  }

  private def trace$(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: RayIntersection, phit: Vector3D,
                     amb: Double, spec: Double, curMaterial: MaterialEta = MaterialEta.AIR): Color = {
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
    trace(phit, objs, ris, depth - 1, nobj, rayIntersections.isEmpty, MaterialEta.AIR) scalarMultiply .75
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
    trace(phit, objs, iobjs, depth - 1, MyRay.seekNearestObj(iobjs), rayIntersections.isEmpty, nearestIntersection.obj.materialEta)
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
        (MaterialEta.AIR.eta, MaterialEta.GLASS.eta)
      case _ =>
        (MaterialEta.GLASS.eta, MaterialEta.AIR.eta)
    }
    (eta1, eta2)
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


  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)


}
