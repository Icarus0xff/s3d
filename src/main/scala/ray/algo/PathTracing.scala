package ray.algo

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Ray
import ray.common.Ray.RayIntersection
import ray.scenes.LightDraw

import scala.util.Random


object PathTracing{
  val scene = LightDraw
  private val xi = 1900
  private val yi = 1500


  val width = 0 until xi toArray
  val height = 0 until yi toArray


  val eye = new Vector3D(xi / 2, yi / 2, -700f)
  val light = scene.light


  def outputRenderedPic = {
    val pixs = for {
      x <- width
      y <- height
    } yield {
      (x, y)
    }


    val newBufferedImage = new BufferedImage(width.size, height.size, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until xi
      y <- 0 until yi
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }

    import ray.common.Utils._
    val randomRays = for {
      pix <- pixs


    } yield {
      val zz = for {
        _ <- 1 to 2
        r = Random.nextDouble()
        xx = pix._1 + r
        yy = pix._2 + r
        randomDir: Vector3D = (new Vector3D(xx, yy, 0) subtract eye).normalize()
        rayIntersections: Array[RayIntersection] = Ray.rayIntersections(eye, randomDir, scene.sceneObjs)
        ri <- rayIntersections
      } yield {

        val color: Vector3D = Phong.renderPix(eye, randomDir, ri.distance, light, ri.obj, .5, .5, ri.obj.color)

        color.scalarMultiply(1 / 2)
      }

      val cc: Color = zz.reduce { (l, r) => l add r }

      (pix._1, pix._2, cc)

    }

    randomRays.foreach {
      pix =>
        newBufferedImage.setRGB(pix._1, pix._2, pix._3.getRGB)
    }


    val file = new File("pic.png")
    ImageIO.write(newBufferedImage, "PNG", file)
  }


}
