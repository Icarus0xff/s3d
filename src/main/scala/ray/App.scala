package ray

import java.awt.Color
import java.io.File

object App{

  val INF: Double = 1000000000000f


  val height = 1 to 700 toArray
  val width = 1 to 799 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(700, 400, 200f), 128f)
  val light = Sphere(Vec3f(400, 200, 100f), 1)

  def main(args: Array[String]): Unit = {
    rayTrace
  }

  private def rayTrace = {
    val pixs = for {
      w <- width
      h <- height
    } yield {
      (w, h)
    }


    val ps = render(pixs, eye, sphere)

    val file = new File("pic2.bmp")
    import java.awt.image.BufferedImage
    val newBufferedImage = new BufferedImage(800, 800, BufferedImage.TYPE_INT_RGB)

    ps.foreach {
      x =>
        newBufferedImage.setRGB(x._1, x._2, x._3.getRGB)
    }


    import javax.imageio.ImageIO
    ImageIO.write(newBufferedImage, "BMP", file)


    val file1 = new File("pic3.bmp")
    import java.awt.image.BufferedImage
    val newBufferedImage1 = new BufferedImage(800, 800, BufferedImage.TYPE_INT_RGB)

    ps.filter(x => x._3 == Color.BLUE).foreach {
      x =>
        newBufferedImage1.setRGB(x._1, x._2, x._3.getRGB)
    }

    import javax.imageio.ImageIO
    ImageIO.write(newBufferedImage1, "BMP", file1)
  }

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, sphere: Sphere): Array[(Int, Int, Color)] = {
    pixs.map {
      curPix =>
        val eyeToPix = computeRay(eye, curPix, sphere)
        val firstIntersect = sphere.intersect(eye, eyeToPix)

        val color = firstIntersect match {
          case (true, near) =>
            val pHit = eye + (eyeToPix * near)
            val pToLightDir = (light.center - pHit).norm
            val secondIntersect = sphere.intersect(pHit, pToLightDir)


            println(pHit)

            val c = secondIntersect match {
              case (true, _) =>
                Color.BLUE
              case _ =>

                val ambientStrength = .1
                val lightColor = Vec3f(255, 255, 255)
                val ambient = lightColor * ambientStrength

                val norm = (pHit - sphere.center).norm
                val lightDir = (pHit - light.center) norm
                val diff = Math.max(lightDir dot norm, 0)
                val diffuse = lightColor * diff

                val specularStrength = 0.7

                val viewDir = (pHit - eye) norm
                val reflectDir = norm * (2 * (lightDir dot norm)) - lightDir

                val spec = Math.pow(Math.max(viewDir dot reflectDir, 0.0), 32)
                val specular = lightColor * (specularStrength * spec)

                val result = (ambient + diffuse + specular) * Vec3f(.7, 1, .7)

                //                println(s"res: $result ambient: $ambient diffuse: $diffuse specular: $specular")

                //val c = new Color(result.x.toInt % 256, result.y.toInt % 256, result.z.toInt % 256)
                new Color(result.x.toInt, result.y.toInt, result.z.toInt)
              //c

            }
            c
          case _ => Color.BLACK
        }

        (curPix._1, curPix._2, color)
    }
  }

  private def computeRay(eye: Vec3f, xy: (Int, Int), s: Sphere) = {
    val p = Vec3f(xy._1, xy._2, 0)
    val dir = (p - eye)


    dir norm

  }


  case class Vec3f(x: Double, y: Double, z: Double){

    def -(that: Vec3f) = Vec3f(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Vec3f) = Vec3f(this.x + that.x, this.y + that.y, this.z + that.z)

    def +(that: Double) = Vec3f(this.x + that, this.y + that, this.z + that)

    def *(that: Vec3f) = Vec3f(this.x * that.x, this.y * that.y, this.z * that.z)

    def *(that: Double) = Vec3f(x * that, y * that, z * that)

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


    def intersect(o: Vec3f, u: Vec3f): (Boolean, Double) = {
      val L = center - o

      val tca = L dot u
      if (tca < 0) {
        return (false, 0)
      }

      val d2 = (L dot L) - tca * tca
      if (d2 > radius2) {
        return (false, 0)
      }

      val thc = Math.sqrt(radius2 - d2);
      val t0 = tca - thc
      val t1 = tca + thc

      (t0, t1) match {
        case (a, b) if a < 0 && b < 0 =>
          (false, 0)
        case (a, b) if a > b =>
          (true, a)
        case (a, b) if a < b =>
          (true, b)
      }
    }

  }

}
