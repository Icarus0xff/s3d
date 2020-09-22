package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.Material.Material
import ray.common.{Material, Object3D, Surface}
import ray.scenes.Scene2


object App{
  val scene = Scene2
  val height = 1 to 1500 toArray
  val width = 1 to 1600 toArray

  val eye = new Vector3D(width.size / 2, height.size / 2, -900f)
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


    val hw = 1700
    val newBufferedImage = new BufferedImage(hw, hw, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 to hw - 1
      y <- 0 to hw - 1
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    renderScene(pixs, eye, scene.scene).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): IntersectStatus = {
    IntersectStatus(s.dir, s.d, s.obj)
  }

  case class IntersectStatus(originDir: Vector3D, distance: Double, obj: Object3D)

  case class PixIntersectedObj(pix: (Int, Int), dir: Vector3D, d: Double, obj: Object3D)

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
                    curMaterial: Material): Color = {
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
        //Color.YELLOW
        trace$(eye, objs, depth, nearestObj, phit, .01, .01, curMaterial)
    }

    c
  }

  private def trace$(eye: Vector3D, objs: Set[Object3D], depth: Int, nearestIntersection: IntersectStatus, phit: Vector3D,
                     amb: Double, spec: Double, curMaterial: Material): Color = {
    import ray.common.Utils._
    lazy val n = nearestIntersection.obj.normal(phit)

    nearestIntersection.obj.surface match {
      case Surface.REFLECTIVE =>
        val reflect = nearestIntersection.originDir.subtract(n scalarMultiply (2 * (n dotProduct nearestIntersection.originDir)))

        val intersectStatuses = for {
          obj <- objs diff Set(nearestIntersection.obj)
          intersection = obj.intersect(phit, reflect)
          if intersection._1
        } yield IntersectStatus(reflect, intersection._2, obj)

        trace(phit, objs, intersectStatuses.toArray, depth - 1, curMaterial) scalarMultiply .5
      case Surface.REGULAR => Phong.renderPix(eye, nearestIntersection.originDir, nearestIntersection.distance, light, nearestIntersection.obj, amb, spec, nearestIntersection.obj.color)
      case Surface.REFRACTIVE =>
        val viewDir = phit subtract eye


        val costheta = viewDir.normalize() dotProduct nearestIntersection.obj.normal(phit)

        val theta = Math.acos(costheta)

        val (n1, n2) = theta match {
          case v if v > -0.5 * Math.PI && v < 0.5 * Math.PI => (Material.AIR.eta, Material.GLASS.eta)
          case _ =>

            (Material.GLASS.eta, Material.AIR.eta)
        }


        val tt = (viewDir add (n scalarMultiply (n dotProduct viewDir))) scalarMultiply (n1 / n2)
        val tt1 = 1 - (n1 * n1) / (n2 * n2) * (1 - Math.pow(viewDir dotProduct n, 2))
        val tt2 = n scalarMultiply Math.sqrt(tt1)
        val refractDir = (tt subtract tt2) normalize


        val phit1 = phit add (nearestIntersection.originDir scalarMultiply 16)

        val intersectStatuses = for {
          obj <- objs
          intersection = obj.intersect(phit1, refractDir)
          if intersection._1
        } yield IntersectStatus(refractDir, intersection._2, obj)


        trace(phit, objs, intersectStatuses.toArray, depth - 1, nearestIntersection.obj.material)


      case Surface.LIGHT => Color.WHITE

    }
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


  case class IntersectResult(is: Boolean, t0: Double, t1: Double)


}
