package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.Utils.Vec3f
import ray.common.{Object3D, Sphere, Triangle, Utils}

object App{
  val height = 1 to 1400 toArray
  val width = 1 to 1500 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(700, 700, 400f), 200f, new Vector3D(.25, .45, .07))
  val sphere1 = Sphere(Vec3f(200, 700, 200f), 256f, new Vector3D(.5, .5, .5))
  val sphere2 = Sphere(Vec3f(300, 200, 250f), 40, new Vector3D(.25, .45, .07))

  val light = Sphere(Vec3f(700, 100, 200f), 1, new Vector3D(.25, .45, .07))


  private val large = 1000000000
  val triangle = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 1700), //b
    Vec3f(2400, 1200, 1700), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangle1 = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(2400, 1200, 1700), //c
    Vec3f(2400, -1200, 1700), //b
    color = new Vector3D(0, .6, 0)
  )

  val floor = Triangle(
    Vec3f(100, 1300, -100), //a
    Vec3f(1400, 1300, -100), //b
    Vec3f(100, 1300, 1400), //c
    color = new Vector3D(.5, .5, .02)
  )


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


    val newBufferedImage = new BufferedImage(1600, 1600, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 to 1599
      y <- 0 to 1599
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    render(pixs, eye, Set(triangle1, triangle, floor, sphere1, sphere)).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }


  case class PixIntersectedObj(pix: (Int, Int), isIntersect: Boolean, dir: Vec3f, d: Double, obj: Object3D)

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

    import Utils._

    val pixToEyePathAllIntersectedObjs = (for {
      pix <- pixs
      obj <- objs

      eyeToPix = computeRay(eye, pix)
      intersection = obj.intersect(eye, eyeToPix)

      if intersection._1
    } yield PixIntersectedObj(pix, intersection._1, eyeToPix, intersection._2, obj)).groupBy(x => x.pix)

    val pixColor = for {
      pixIntersection <- pixToEyePathAllIntersectedObjs
      intersectedObjs = pixIntersection._2

      nearestObj = intersectedObjs.reduce {
        (a, b) =>
          if (a.d < b.d) {
            a
          } else {
            b
          }
      }


      phit: Vec3f = eye add (nearestObj.dir scalarMultiply (nearestObj.d))
      phitToLight = light.center subtract phit
      maxDistance = light.center.distance(phit)
      hitToLightDir: Vec3f = phitToLight normalize()

      otherObjs = objs diff Set(nearestObj.obj)

      notIntersectedObjs = otherObjs.takeWhile(x => {
        val r = x.intersect(phit, hitToLightDir)
        r._1 == false || r._2 > maxDistance
      }
      )
    } yield {

      val c = otherObjs.size - notIntersectedObjs.size match {
        case 0 => Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .3, 0.8)
        case _ =>
          Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .05, .1)
      }

      (pixIntersection._1._1, pixIntersection._1._2, c)
    }

    pixColor toArray

  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    val dir = (pix - eye)


    dir norm

  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
