package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.MaterialEta.MaterialEta
import ray.common.Ray.RayIntersection
import ray.common.{MaterialEta, Object3D, Ray, Surface}
import ray.scenes.LightDraw


object App{
  val scene = LightDraw
  private val xi = 1400
  private val yi = 1000


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
      (rayIntersection._1._1, rayIntersection._1._2, trace(eye, objs, rayIntersections, 8, seekNearestObj(rayIntersections), rayIntersections.size == 0, MaterialEta.AIR))
    }

    pixColor toArray

  }

  private def trace(eye: Vector3D, objs: Set[Object3D], intersectedObjs: Array[RayIntersection], depth: Int, renderObj: RayIntersection,
                    isNoIntersect: Boolean, curMaterial: MaterialEta = MaterialEta.AIR): Color = {
    if (depth == 0 || isNoIntersect) {
      return Color.GRAY
    }

    val otherObjs = objs diff Set(renderObj.obj)

    val phit: Vector3D = eye add (renderObj.originDir scalarMultiply renderObj.distance)
    val phitToLight = light.center subtract phit
    val maxDistance = light.center.distance(phit)
    val phitToLightDir: Vector3D = phitToLight normalize()
    val notIntersectedObjs = otherObjs.takeWhile { object3D =>
      val r = object3D.intersect(phit, phitToLightDir)
      r._1 == false || r._2 > maxDistance
    }

    val intersections2nd = Ray.intersect(phit, phitToLight normalize, objs, renderObj.obj).filter(x => x.distance < maxDistance)

    //    val cao = for {
    //      is <- intersections2nd
    //      o = is.obj
    //      if o.surface == Surface.REFRACTIVE
    //    } yield {
    //
    //    }


    val c = intersections2nd.isEmpty match {
      case true =>
        trace$(eye, objs, depth, renderObj, phit, .4, .6, curMaterial)
      case false =>
        // in shadow
        //val glass = intersections2nd.filter(x => x.obj.surface == Surface.REFRACTIVE)
        //        println(s"cainima ${intersections2nd.size} $intersections2nd")
        val nobj = seekNearestObj(intersections2nd.toArray)
        //        println(s"cainima end")


        nobj.obj.surface match {
          case Surface.REFRACTIVE =>
            //            //            println("fuck")
            val phit1 = phit subtract (nobj.originDir scalarMultiply 16)
            //            //Color.BLACK
            //            val c = trace$(phit1, objs, depth, nobj, phit, .01, .01, curMaterial)
            //            println(c)
            //            c

            val shabima = phit add (nobj.originDir scalarMultiply nobj.distance)

            import ray.common.Utils._
            val col1: Vector3D = computeRefraction(phit1, objs, depth, nobj, shabima, nobj.obj.normal(phit))
            val col2: Vector3D = trace$(eye, objs, depth, renderObj, phit, .01, .01, curMaterial)
            val cc: Color = col1 scalarMultiply .3 add col2
            cc

          //trace$(phit1, objs, depth, nobj, phit, .6, .5, curMaterial)
          case _ => trace$(eye, objs, depth, renderObj, phit, .01, .01, curMaterial)
        }


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

    val reflectIs = Ray.intersect(phit, reflect, objs, nearestIntersection.obj)

    val ris = reflectIs.toArray
    val nobj = seekNearestObj(ris)
    trace(phit, objs, ris, depth - 1, nobj, reflectIs.isEmpty, MaterialEta.AIR) scalarMultiply .75
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
    trace(phit, objs, iobjs, depth - 1, seekNearestObj(iobjs), rayIntersections.isEmpty, nearestIntersection.obj.materialEta)
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

  private def seekNearestObj(intersectedObjs: Array[RayIntersection]) = {
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


  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)


}
