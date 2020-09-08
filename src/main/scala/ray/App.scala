package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import ray.algo.Phong
import ray.common.Object3D
import ray.common.Utils.{Plane, Sphere, Vec3f}

object App{
  val height = 1 to 1400 toArray
  val width = 1 to 1500 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(1000, 900, 200f), 256f)
  val light = Sphere(Vec3f(400, 200, 1000f), 1)

  val floor = Plane(Vec3f(0, -1, 0), 1)

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

    render(pixs, eye, List(floor, sphere)).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, objs: List[Object3D]): Array[(Int, Int, Color)] = {
    for {
      pix <- pixs
      obj <- objs
      eyeToPix = computeRay(eye, pix)
      color = obj.intersect(eye, eyeToPix) match {
        case (true, d) =>
          Phong.renderPix(eye, eyeToPix, d, light, obj)
        case _ => Color.BLACK
      }

    } yield (pix._1, pix._2, color)
  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    val dir = (pix - eye)


    dir norm

  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
