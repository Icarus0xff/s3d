package ray

import java.awt.Color
import java.io.File

import ray.App.Shadow.Shadow

object App{

  val INF: Double = 1000000000000f

  object Shadow extends Enumeration{
    type Shadow = Value
    val Shadow, BackColor, Color = Value
  }

  val height = 1 to 700 toArray
  val width = 1 to 700 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -10000f)
  val sphere = Sphere(Vec3f(width.size / 2, height.size / 2, 100f), 80f)
  val light = Sphere(Vec3f(width.size, height.size, 10f), 1f)

  def main(args: Array[String]): Unit = {
    rayTrace
  }

  private def rayTrace = {
    val pixs = for {
      h <- height
      w <- width
    } yield {
      (h, w)
    }


    val ps = render(pixs, eye, sphere)

    val file = new File("pic1.bmp")
    import java.awt.image.BufferedImage
    val newBufferedImage = new BufferedImage(800, 800, BufferedImage.TYPE_INT_RGB)


    ps.foreach {
      x =>
        x._3 match {
          case Shadow.BackColor => newBufferedImage.setRGB(x._1, x._2, Color.WHITE.getRGB)
          case Shadow.Color => newBufferedImage.setRGB(x._1, x._2, Color.GREEN.getRGB)
          case Shadow.Shadow => newBufferedImage.setRGB(x._1, x._2, Color.BLACK.getRGB)
        }
    }

    import javax.imageio.ImageIO
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, sphere: Sphere): Array[(Int, Int, Shadow)] = {
    pixs.map {
      curPix =>

        val rayDir = computeRay(eye, curPix, sphere, height.size, width.size)
        println(s"raydir: $rayDir")

        val intersectResult = sphere.intersect(eye, rayDir)

        val shadow = intersectResult match {
          case (true, near, far) =>
            val pHit = rayDir + near
            val rd = light.center - pHit

            val ir = sphere.intersect(pHit, rd.norm())
            ir match {
              case (true, _, _) => Shadow.Shadow
              case (false, _, _) => Shadow.Color
            }
          case _ => Shadow.BackColor
        }

        (curPix._1, curPix._2, shadow)
    }
  }

  private def computeRay(eye: Vec3f, yx: (Int, Int), s: Sphere, m: Int, k: Int) = {
    val c = Vec3f(yx._2, yx._1, 0)

    val dir = (c - eye)
    println(dir)

    dir norm

  }


  case class Vec3f(x: Double, y: Double, z: Double){

    def -(that: Vec3f) = Vec3f(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Vec3f) = Vec3f(this.x + that.x, this.y + that.y, this.z + that.z)

    def +(that: Double) = Vec3f(this.x + that, this.y + that, this.z + that)

    def *(that: Vec3f) = Vec3f(this.x * that.x, this.y * that.y, this.z * that.z)

    def *(that: Double) = Vec3f(this.x * that, this.y * that, this.z * that)

    def dot(that: Vec3f) = this.x * that.x + this.y * that.y + this.z * that.z


    def norm() = {
      val nor2 = x * x + y * y + z * z

      val normed = nor2 match {
        case _ if nor2 > 0 =>
          val invNor = 1 / Math.sqrt(nor2)
          Vec3f(x * invNor, y * invNor, z * invNor)
        case _ => this
      }

      normed
    }
  }

  case class IntersectResult(is: Boolean, t0: Double, t1: Double)

  case class Sphere(center: Vec3f, radius: Double){

    val radius2: Double = radius * radius
    val falseR = IntersectResult(false, INF, INF)

    def intersect(o: Vec3f, u: Vec3f): (Boolean, Double, Double) = {
      val t = o - center
      val ∇ = Math.pow((u dot t), 2) - (t.dot(t) - radius2)

      val d = -(u dot t)
      println(s"∇: ${∇} d: $d")


      if (∇ > 0) {
        (true, d - ∇, d + ∇)
      } else if (∇ == 0) {
        (true, d, d)
      } else {
        (false, d, d)
      }
    }

  }

}
