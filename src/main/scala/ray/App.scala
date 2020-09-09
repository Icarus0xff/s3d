package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import ray.algo.Phong
import ray.common.Object3D
import ray.common.Utils.{Plane, Sphere, Triangle, Vec3f}

object App{
  val height = 1 to 1400 toArray
  val width = 1 to 1500 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(1000, 900, 200f), 256f)
  val sphere1 = Sphere(Vec3f(200, 700, 200f), 256f)
  val light = Sphere(Vec3f(400, 200, 1000f), 1)

  private val large = 1005
  val triangle = Triangle(
    Vec3f(500, 100, 1000), //a
    Vec3f(100, 500, 1000), //b
    Vec3f(309, 400, 1000), //c
  )

  val floor = Plane(Vec3f(0, 0, -1), 1)

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

    render(pixs, eye, List(triangle, sphere1, sphere)).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, objs: List[Object3D]): Array[(Int, Int, Color)] = {
    val pixIntersections = (for {
      pix <- pixs

      obj <- objs

      eyeToPix = computeRay(eye, pix)
      intersection = obj.intersect(eye, eyeToPix)
      if intersection._1

    } yield (pix, intersection._1, intersection._2, obj, eyeToPix)).groupBy(x => x._1)

    val pixNearestObj = for {
      is <- pixIntersections
      body = is._2


      nearestObj = body.reduce {
        (a, b) =>
          if (a._3 < b._3) {
            a
          } else {
            b
          }
      }

      c = Phong.renderPix(eye, nearestObj._5, nearestObj._3, light, nearestObj._4)
      //c = Color.BLUE


    } yield (is._1._1, is._1._2, c)

    pixNearestObj toArray

  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    val dir = (pix - eye)


    dir norm

  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
