package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.Utils.{Sphere, Triangle, Vec3f}
import ray.common.{Object3D, Utils}

object App{
  val height = 1 to 1400 toArray
  val width = 1 to 1500 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(700, 700, 400f), 200f, new Vector3D(.25, .45, .07))
  val sphere1 = Sphere(Vec3f(200, 700, 200f), 256f, new Vector3D(.5, .5, .5))
  val sphere2 = Sphere(Vec3f(300, 200, 250f), 40, new Vector3D(.25, .45, .07))

  val light = Sphere(Vec3f(400, 200, 600f), 1, new Vector3D(.25, .45, .07))
  val light1 = Sphere(Vec3f(420, 220, 620f), 10, new Vector3D(.25, .45, .07))

  private val large = 1000000000
  val triangle = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 1700), //b
    Vec3f(2400, 1200, 1700), //c
    color = new Vector3D(.6, 0, .6)
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

    render(pixs, eye, Set(triangle, floor, sphere1, sphere)).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }


  case class Result(pix: (Int, Int), isIntersect: Boolean, dir: Vec3f, d: Double, obj: Object3D)

  private def render(pixs: Array[(Int, Int)], eye: Vec3f, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

    import Utils._

    val pixIntersections = (for {
      pix <- pixs
      obj <- objs

      eyeToPix = computeRay(eye, pix)
      intersection = obj.intersect(eye, eyeToPix)
      if intersection._1

    } yield Result(pix, intersection._1, eyeToPix, intersection._2, obj)).groupBy(x => x.pix)

    val pixNearestObj = for {
      is <- pixIntersections
      results = is._2

      nearestObj = results.reduce {
        (a, b) =>
          if (a.d < b.d) {
            a
          } else {
            b
          }
      }


      phit: Vec3f = eye add (nearestObj.dir scalarMultiply (nearestObj.d))
      htoL: Vec3f = (light.center subtract phit) normalize()


    } yield {

      val fuckni = for {
        otherObjs <- objs diff Set(nearestObj.obj)
        secondIntersection = otherObjs.intersect(phit, htoL)
        if secondIntersection._1
      } yield secondIntersection

      val allfuck = fuckni.foldLeft((false, 0)) { (l, r) => (l._1 || r._1, 0) }

      val c = allfuck match {
        case (true, 0) =>
          Color.BLACK
        //Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, 0, 0)
        case _ => Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .2, 0.8)
      }


      (is._1._1, is._1._2, c)
    }

    pixNearestObj toArray

  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    val dir = (pix - eye)


    dir norm

  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
