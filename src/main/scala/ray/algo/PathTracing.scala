package ray.algo

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import ray.common.Ray.RayIntersection
import ray.common._
import ray.scenes.LightDraw

import scala.util.Random


object PathTracing{
  val scene = LightDraw
  private val width = 1200
  private val height = 1000


  val xi = 0 until width toArray
  val yi = 0 until height toArray


  val eye = new Vector3D(width / 2, height / 2, -700f)
  val light = scene.light


  def outputRenderedPic = {
    val pixs = for {
      x <- xi
      y <- yi
    } yield {
      (x, y)
    }


    val newBufferedImage = new BufferedImage(xi.size, yi.size, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    import ray.common.Utils._
    val MAX_RANDOM_RAY_DIR = 16

    val randomRays = for {
      pix <- pixs
    } yield {

      val colorPart = for {
        _ <- 1 to MAX_RANDOM_RAY_DIR
        r = Random.nextDouble()
        xx = pix._1 + r
        yy = pix._2 + r
        randomDir: Vector3D = (new Vector3D(xx, yy, 0) subtract eye) normalize()
        rayIntersections: Array[RayIntersection] = Ray.rayIntersections(eye, randomDir, scene.sceneObjs)
        ri = Ray.seekNearestObj(rayIntersections)
      } yield {
        val phit = eye add (randomDir scalarMultiply ri.distance)

        val color: Vector3D = shade(ri, phit, randomDir) scalarMultiply (1f / MAX_RANDOM_RAY_DIR)

        color
      }

      val color: Vector3D = colorPart.reduce { (l, r) => l add r }
      if (color.getBlue > 250) {
        println(color)
      }
      (pix._1, pix._2, color)
    }


    randomRays.foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.png")
    ImageIO.write(newBufferedImage, "PNG", file)
  }


  private def shade(ri: RayIntersection, phit: Vector3D, dir: Vector3D): Vector3D = {

    val uniformPointOnLight = light.uniformSample()
    val tdir = (phit subtract uniformPointOnLight) normalize
    val bias = tdir scalarMultiply 16
    val aPoint = uniformPointOnLight add bias
    val rayIntersections: Array[RayIntersection] = Ray.intersect(aPoint, tdir, Set(ri.obj)).toArray


    val cosTheta$ = tdir.dotProduct(light.normal(uniformPointOnLight))

    val c: Vector3D = cosTheta$ match {
      case v if v > 0 && rayIntersections.size > 0 =>
        val cosTheta = FastMath.max(-tdir.dotProduct(ri.obj.normal(phit)), 0)


        val fr = ri.obj.color
        val liMultiFr = Utils.multi(fr scalarMultiply 255, light.color scalarMultiply 255) scalarMultiply cosTheta scalarMultiply cosTheta$
        val lDir = liMultiFr scalarMultiply (1f / (uniformPointOnLight distanceSq phit)) scalarMultiply (light.surfaceArea / 32)


        lDir
      case _ => Vector3D.ZERO
    }

    c


  }
}
