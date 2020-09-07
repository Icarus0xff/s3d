package zzz

import java.awt.Color
import java.io.File

object App{

  val INF: Double = 1000000000000f
  val wVec = Vec3f(0, 1, 0)

  def main(args: Array[String]): Unit = {


    rayTrace

  }

  private def rayTrace = {
    val height = 1 to 400 toArray
    val width = 1 to 400 toArray


    val pixs = for {
      h <- height
      w <- width
    } yield {
      (h, w)
    }

    val eye = Vec3f(width.size / 2, height.size / 2, -100f)
    val sphere = Sphere(Vec3f(width.size / 2, height.size / 2, 100f), 64f)


    val ps = pixs.map {
      curPix =>

        val rayDir = computeRay2(eye, curPix, sphere, height.size, width.size)
        println(s"raydir: $rayDir")

        val rs = sphere.intersect1(eye, rayDir)


        (curPix._1, curPix._2, rs)
    }

    val file = new File("pic1.bmp")
    import java.awt.image.BufferedImage
    val newBufferedImage = new BufferedImage(800, 800, BufferedImage.TYPE_INT_RGB)


    ps.foreach {
      x =>
        x._3 match {
          case true => newBufferedImage.setRGB(x._1, x._2, Color.GREEN.getRGB)
          case false => newBufferedImage.setRGB(x._1, x._2, Color.WHITE.getRGB)
        }
    }

    import javax.imageio.ImageIO
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  private def computeRay2(eye: Vec3f, yx: (Int, Int), s: Sphere, m: Int, k: Int) = {
    val c = Vec3f(yx._2, yx._1, 0)

    val dir = (c - eye)
    println(dir)

    dir norm

  }

  private def computeRay1(eye: Vec3f, hwJi: (Int, Int), s: Sphere, m: Int, k: Int) = {


    val t = Vec3f(200, 200, 1000) - eye
    val tn = t norm

    val b = wVec * t
    val bn = b norm


    val vn = tn * bn


    println(s"t: ${t} tn: ${tn} bn: ${bn}, vn: ${vn}")

    println(s"tDotB: ${tn dot bn} bnDotVn: ${bn dot vn} z: ${tn dot vn}")

    println(s"tDotB: ${t dot b}")


    val gx = Math.tan(0.785398163)
    val gy = gx * (m / k)

    val qx = bn * ((2 * gx) / (k - 1))
    val qy = vn * ((2 * gy) / (m - 1))
    val p1m = tn - (bn * gx) - (vn * gy)

    val pij = p1m + (qx * (hwJi._2 - 1)) + (qy * (hwJi._1 - 1))
    println(s"pij: $pij")

    val rayDir = pij norm

    rayDir
  }

  private def computeRay(eye: Vec3f, pix: (Int, Int), s: Sphere, m: Int, k: Int) = {


    val t = s.center - eye
    val tn = t.norm()

    val b = wVec * t
    val bn = b.norm()

    val vn = tn * bn

    println(s"t: ${tn} b: ${bn}, v: ${vn}")

    val gx = Math.tan(0.7854)


    val gy = gx * (m / k)

    val qx = bn * ((2 * gx) / (k - 1))
    val qy = vn * (2 * gy / (m - 1))
    val p1m = tn - vn * gx - vn * gy

    val pij = p1m + qx * (pix._2) + qy * pix._1
    println(s"pij: $pij")

    val rayDir = pij.norm()

    rayDir
  }

  case class Vec3f(x: Double, y: Double, z: Double){

    def -(that: Vec3f) = Vec3f(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Vec3f) = Vec3f(this.x + that.x, this.y + that.y, this.z + that.z)

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

    def intersect(rayOrigin: Vec3f, rayDir: Vec3f): IntersectResult = {
      val l = center - rayOrigin
      val tca = l dot rayDir


      if (tca < 0) {
        return falseR
      }
      val d2 = (l dot l) - tca * tca
      println(s"d2: $d2")
      if (d2 > radius2) {
        return falseR
      }


      val thc = Math.sqrt(radius2 - d2)
      IntersectResult(true, tca - thc, tca + thc)
    }

    def intersect1(o: Vec3f, u: Vec3f): Boolean = {
      val t = o - center
      val ∇ = Math.pow((u dot (o - center)), 2) - (t.dot(t) - radius2)

      println(s"∇: ${∇}")

      if (∇ >= 0) {

        true
      } else {
        false
      }
    }

  }

}
