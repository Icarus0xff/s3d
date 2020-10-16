package ray.common

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import com.badlogic.gdx.math.Vector3
import javax.imageio.ImageIO
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath


object Utils{


  implicit def vector3DToColor(s: Vector3D): Color = {
    val t = Seq(s.getX toInt, s.getY toInt, s.getZ toInt).map(x => {
      if (x > 255) {
        255
      } else {
        x
      }
    })
    new Color(t(0), t(1), t(2))
  }

  implicit def colorToVector3D(s: Color): Vector3D = new Vector3D(s.getRed, s.getGreen, s.getBlue)

  implicit def vector3DToVector3(s: Vector3D): Vector3 = new Vector3(s.getX.toFloat, s.getY.toFloat, s.getZ.toFloat)

  implicit def vector3ToVector3D(s: Vector3): Vector3D = new Vector3D(s.x, s.y, s.z)


  def multi(a: Vector3D, b: Vector3D): Vector3D = {
    new Vector3D(
      a.getX * b.getX,
      a.getY * b.getY,
      a.getZ * b.getZ,
    )
  }

  def outputImage(width: Int, height: Int, pixColor: Array[(Int, Int, Int)]): Boolean = {
    val newBufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      newBufferedImage.setRGB(x, y, Color.GRAY.getRGB)
    }
    pixColor.foreach {
      pix => newBufferedImage.setRGB(pix._1, pix._2, pix._3)
    }


    val file = new File("pic.png")
    ImageIO.write(newBufferedImage, "PNG", file)
  }

  def unifromSampleHemisphere(u: Double, v: Double): Vector3D = {
    val sinTheta = FastMath.sqrt(-u * (u - 2));
    val theta = FastMath.asin(sinTheta)
    val phi = 2 * FastMath.PI * v;
    new Vector3D(sinTheta * FastMath.cos(phi), sinTheta * FastMath.sin(phi), u)

    //    val theta = FastMath.acos(FastMath.pow(1 - u, 1 / 2))
    //    val phi = 2 * FastMath.PI * v
    //
    //
    //    val x = FastMath.sin(theta) * FastMath.cos(phi)
    //    val y = FastMath.sin(theta) * FastMath.sin(phi)
    //    val z = FastMath.cos(theta)
    //
    //    new Vector3D(x, y, z)


  }
}
