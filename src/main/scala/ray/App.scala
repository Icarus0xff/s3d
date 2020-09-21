package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import ray.algo.Phong
import ray.common.Material.Material
import ray.common.Utils.Vec3f
import ray.common.{Material, Object3D, Surface}
import ray.scenes.Scene2


object App{
  val scene = Scene2
  val height = 1 to 1500 toArray
  val width = 1 to 1600 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -900f)
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

  case class IntersectStatus(originDir: Vec3f, distance: Double, obj: Object3D)

  case class PixIntersectedObj(pix: (Int, Int), dir: Vec3f, d: Double, obj: Object3D)

  private def renderScene(pixs: Array[(Int, Int)], eye: Vec3f, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

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
      (rayIntersection._1._1, rayIntersection._1._2, trace(eye, objs, statuses, 3))
    }

    pixColor toArray

  }

  private def trace(eye: Vec3f, objs: Set[Object3D], intersectedObjs: Array[IntersectStatus], depth: Int): Color = {
    if (depth == 0 || intersectedObjs.size == 0) {
      return Color.GRAY
    }
    val nearestObj = seekNearestObj(intersectedObjs)
    val otherObjs = objs diff Set(nearestObj.obj)

    val phit: Vec3f = eye add (nearestObj.originDir scalarMultiply (nearestObj.distance))
    val phitToLight = light.center subtract phit
    val maxDistance = light.center.distance(phit)
    val hitToLightDir: Vec3f = phitToLight normalize()
    val notIntersectedObjs = otherObjs.takeWhile { object3D =>
      val r = object3D.intersect(phit, hitToLightDir)
      r._1 == false || r._2 > maxDistance
    }

    val c = otherObjs.size - notIntersectedObjs.size match {
      case 0 =>
        trace$(eye, objs, depth, nearestObj, phit, .4, .6)
      case _ =>
        trace$(eye, objs, depth, nearestObj, phit, .01, .01)
    }

    c
  }

  private def trace$(eye: Vec3f, objs: Set[Object3D], depth: Int, nearestIntersection: IntersectStatus, phit: Vec3f,
                     amb: Double, spec: Double, curMaterial: Material = Material.NONE): Color = {
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

        trace(phit, objs, intersectStatuses.toArray, depth - 1) scalarMultiply .5
      case Surface.REGULAR => Phong.renderPix(eye, nearestIntersection.originDir, nearestIntersection.distance, light, nearestIntersection.obj, amb, spec, nearestIntersection.obj.color)
      case Surface.REFRACTIVE =>
        val n1 = 1
        val n2 = 1.52

        val viewDir = phit subtract eye
        val tt = (viewDir add (n scalarMultiply (n dotProduct viewDir))) scalarMultiply (n1 / n2)
        val tt1 = 1 - (n1 * n1) / (n2 * n2) * (1 - Math.pow(viewDir dotProduct n, 2))
        val tt2 = n scalarMultiply Math.sqrt(tt1)
        val t = (tt subtract tt2) normalize


        val intersectStatuses = for {
          obj <- objs diff Set(nearestIntersection.obj)
          intersection = obj.intersect(phit, t)
          if intersection._1
        } yield IntersectStatus(t, intersection._2, obj)


        trace(phit, objs, intersectStatuses.toArray, depth - 1) scalarMultiply .5


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

  private def rayIntersections(pixs: Array[(Int, Int)], eye: Vec3f, objs: Set[Object3D]) = {
    for {
      pix <- pixs
      obj <- objs

      eyeToPix = computeRay(eye, pix)
      intersection = obj.intersect(eye, eyeToPix)

      if intersection._1
    } yield PixIntersectedObj(pix, eyeToPix, intersection._2, obj)
  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    pix - eye normalize
  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
