package ray.common


import com.badlogic.gdx.math.collision.Ray
import com.badlogic.gdx.math.{Intersector, Vector3}
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import ray.common.MaterialEta.MaterialEta
import ray.common.Surface.Surface
import ray.common.Utils._

import scala.annotation.tailrec
import scala.util.Random


case class Sphere(center: Vector3D, radius: Double, color: Vector3D, surface: Surface = Surface.REGULAR, materialEta: MaterialEta = MaterialEta.AIR)
  extends Object3D{

  private val sphere = new com.badlogic.gdx.math.collision.Sphere(center, radius.toFloat)
  val radius2: Double = radius * radius


  override def intersect(o: Vector3D, dir: Vector3D): (Boolean, Double) = {
    val ip = new Vector3()
    val ray = new Ray(o, dir)
    val rs = Intersector.intersectRaySphere(ray, center, radius.toFloat, ip)

    (rs, ip distance o)
  }

  override def normal(p: Vector3D): Vector3D = {
    (p subtract center).normalize()
  }

  override def surfaceArea(): Double = {
    sphere.surfaceArea()
  }


  val r1 = new Random(42)
  val r2 = new Random(43)

  @tailrec
  private def random(): Vector3D = {
    val x1 = r1.nextDouble()
    val x2 = r2.nextDouble()

    val x12 = x1 * x1
    val x22 = x2 * x2
    val sum = x12 + x22

    sum match {
      case v if v <= 1 =>
        val t = FastMath.sqrt(1 - x12 - x22)

        new Vector3D(2 * x1 * t, 2 * x2 * t, 1 - 2 * sum)
      case _ => random
    }


  }

  override def uniformSample(): Vector3D = {
    random scalarMultiply radius add center
  }

}
