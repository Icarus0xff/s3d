package ray.common

import com.badlogic.gdx.math.collision.Ray
import com.badlogic.gdx.math.{Intersector, Vector3}
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MaterialEta.MaterialEta
import ray.common.Surface.Surface
import ray.common.Utils._


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

}
