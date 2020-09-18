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
  val height = 1 to 1500 toArray
  val width = 1 to 1600 toArray

  val eye = Vec3f(width.size / 2, height.size / 2, -900f)
  val smallSphere = Sphere(Vec3f(700, 600, 1000f), 300f, new Vector3D(.25, .4, 0))
  val sphere1 = Sphere(Vec3f(1600, 900, 800f), 256f, new Vector3D(.5, .5, .5))


  val light = Sphere(Vec3f(900, 100, 600f), 10, new Vector3D(.25, .45, .07))


  private val large = 1000000000
  val triangle = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 1700), //b
    Vec3f(2400, 1200, 1700), //c
    color = new Vector3D(.6, 0, .6),
    reflective = false
  )

  val triangleBack = Triangle(
    Vec3f(0, -1200, -1000), //a
    Vec3f(0, 1200, -1000), //b
    Vec3f(2400, 1200, -1000), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangle1 = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(2400, 1200, 1700), //c
    Vec3f(2400, -1200, 1700), //b
    color = new Vector3D(0, .4, 0),
    reflective = false
  )

  val triangleLeft = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 100), //a
    Vec3f(0, 1200, 1700), //a
    color = new Vector3D(0, 0, .6),
    //reflective = true
  )

  val triangleRight = Triangle(
    Vec3f(2400, -1200, 1700), //a
    Vec3f(2400, 1200, 1700), //a
    Vec3f(2400, 1200, 100), //a
    color = new Vector3D(0, .3, .4),
    //reflective = true
  )

  val floor = Triangle(
    Vec3f(0, 1200, 100), //a
    Vec3f(2400, 1200, 1700), //b
    Vec3f(0, 1200, 1700), //c
    color = new Vector3D(.5, .5, .02),
    reflective = true
  )

  val floor1 = Triangle(
    Vec3f(0, 1200, 100), //a
    Vec3f(2400, 1200, 100), //b
    Vec3f(2400, 1200, 1700), //b
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


    val hw = 1700
    val newBufferedImage = new BufferedImage(hw, hw, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 to hw - 1
      y <- 0 to hw - 1
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    renderScene(pixs, eye, Set(triangle1, triangle, triangleLeft, triangleRight, floor, floor1, sphere1, smallSphere)).foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.bmp")
    ImageIO.write(newBufferedImage, "BMP", file)
  }

  implicit def pixIntersectedObjToIntersectInfo(s: PixIntersectedObj): IntersectInfo = {
    IntersectInfo(s.dir, s.d, s.obj)
  }

  case class IntersectInfo(dir: Vec3f, d: Double, obj: Object3D)

  case class PixIntersectedObj(pix: (Int, Int), isIntersect: Boolean, dir: Vec3f, d: Double, obj: Object3D)

  private def renderScene(pixs: Array[(Int, Int)], eye: Vec3f, objs: Set[Object3D]): Array[(Int, Int, Color)] = {

    val pixToEyePathAllIntersectedObjs = rayIntersections(pixs, eye, objs)
      .groupBy(x => x.pix)

    val pixColor = for {
      rayIntersection <- pixToEyePathAllIntersectedObjs


    } yield {
      val intersectInfoes = rayIntersection._2.map(x => {
        val r: IntersectInfo = x
        r
      }

      )
      (rayIntersection._1._1, rayIntersection._1._2, render(eye, objs, intersectInfoes, 3))
    }

    pixColor toArray

  }

  private def render(eye: Vec3f, objs: Set[Object3D], intersectedObjs: Array[IntersectInfo], depth: Int): Color = {
    if (depth == 0 || intersectedObjs.size == 0) {
      return Color.GRAY
    }
    val nearestObj = seekNearestObj(intersectedObjs)
    val otherObjs = objs diff Set(nearestObj.obj)

    val phit: Vec3f = eye add (nearestObj.dir scalarMultiply (nearestObj.d))
    val phitToLight = light.center subtract phit
    val maxDistance = light.center.distance(phit)
    val hitToLightDir: Vec3f = phitToLight normalize()
    val notIntersectedObjs = otherObjs.takeWhile { x =>
      val r = x.intersect(phit, hitToLightDir)
      r._1 == false
    }

    val c = otherObjs.size - notIntersectedObjs.size match {
      case 0 =>
        render2(eye, objs, depth, nearestObj, phit, .4, .8)
      case _ =>
        render2(eye, objs, depth, nearestObj, phit, .01, .01)
    }

    c
  }

  private def render2(eye: Vec3f, objs: Set[Object3D], depth: Int, nearestObj: IntersectInfo, phit: Vec3f, amb: Double, spec: Double) = {
    nearestObj.obj.reflective match {
      case true =>
        val n = nearestObj.obj.normal(phit)
        val reflect = nearestObj.dir.subtract(n scalarMultiply (2 * (n dotProduct nearestObj.dir)))

        val z = for {
          obj <- objs
          intersection = obj.intersect(phit, reflect)
          if intersection._1
        } yield IntersectInfo(reflect, intersection._2, obj)

        import ray.common.Utils._
        val c: Vector3D = render(phit, objs, z.toArray, depth - 1)
        val cc: Color = c scalarMultiply (.5)
        cc
      case false => Phong.renderPix(eye, nearestObj.dir, nearestObj.d, light, nearestObj.obj, amb, spec, nearestObj.obj.color)
    }
  }

  private def seekNearestObj(intersectedObjs: Array[IntersectInfo]) = {
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
