package ray

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.algo.Phong
import ray.common.Utils.Vec3f
import ray.common.{Object3D, Sphere, Triangle}

object App{
  val height = 1 to 1400 toArray
  val width = 1 to 1500 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -800f)
  val sphere = Sphere(Vec3f(700, 700, 400f), 200f, new Vector3D(.25, 0, 0))
  val sphere1 = Sphere(Vec3f(200, 700, 200f), 256f, new Vector3D(.5, .5, .5))


  val light = Sphere(Vec3f(700, 100, 600f), 1, new Vector3D(.25, .45, .07))


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
    color = new Vector3D(.5, .5, .02),
    reflective = true
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
      newBufferedImage.setRGB(x, y, Color.BLUE.getRGB)
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

    val pixToEyePathAllIntersectedObjs = rayIntersections(pixs, eye, objs)
      .groupBy(x => x.pix)

    val pixColor = renderPix(eye, objs, pixToEyePathAllIntersectedObjs)

    pixColor toArray

  }

  private def renderPix(eye: Vec3f, objs: Set[Object3D], rayPathAllIntersectedObjs: Map[(Int, Int), Array[PixIntersectedObj]]) = {
    for {
      rayIntersection <- rayPathAllIntersectedObjs
      intersectedObjs = rayIntersection._2

      nearestObj = seekNearestObj(intersectedObjs)
      otherObjs = objs diff Set(nearestObj.obj)

      phit: Vec3f = eye add (nearestObj.dir scalarMultiply (nearestObj.d))
      phitToLight = light.center subtract phit
      maxDistance = light.center.distance(phit)
      hitToLightDir: Vec3f = phitToLight normalize()
      notIntersectedObjs = otherObjs.takeWhile(x => {
        val r = x.intersect(phit, hitToLightDir)
        r._1 == false
      }
      )
    } yield {
      val c = otherObjs.size - notIntersectedObjs.size match {
        case 0 =>
          nearestObj.obj.reflective match {
            case false => Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .3, 0.8, nearestObj.obj.color)
            case true =>
              val n = nearestObj.obj.normal(phit)
              val reflect = nearestObj.dir.subtract(n scalarMultiply (2 * (n dotProduct nearestObj.dir)))

              val fuck = otherObjs.map(x => {
                val r = x.intersect(phit, reflect)
                (x, r._1, r._2)
              }).reduce {
                (l, r) =>
                  if (l._3 > r._3) {
                    r
                  } else {
                    l
                  }
              }

              println(reflect)
              val c: Color = Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .3, 0.8,
                nearestObj.obj.color add (fuck._1.color scalarMultiply (0.5)))
              //import ray.common.Utils._
              //val c: Color = fuck._1.color scalarMultiply 256
              c

          }
        case _ =>
          // shadow
          val n = nearestObj.obj.normal(phit)
          val reflect = nearestObj.dir.subtract(n scalarMultiply (2 * (n dotProduct nearestObj.dir)))

          val fuck = otherObjs.map(x => {
            val r = x.intersect(phit, reflect)
            (x, r._1, r._2)
          }).reduce {
            (l, r) =>
              if (l._3 > r._3) {
                r
              } else {
                l
              }
          }

          val ppp = nearestObj.obj.color add (fuck._1.color scalarMultiply (0.5))
          val c: Color = Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, .05, .1, ppp)
          c

      }

      (rayIntersection._1._1, rayIntersection._1._2, c)
    }
  }

  private def seekNearestObj(intersectedObjs: Array[PixIntersectedObj]) = {
    intersectedObjs.reduce {
      (a, b) =>
        if (a.d < b.d) {
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
    } yield PixIntersectedObj(pix, intersection._1, eyeToPix, intersection._2, obj)
  }

  private def computeRay(eye: Vec3f, xy: (Int, Int)) = {
    val pix = Vec3f(xy._1, xy._2, 0)
    val dir = (pix - eye)


    dir norm

  }




  case class IntersectResult(is: Boolean, t0: Double, t1: Double)



}
