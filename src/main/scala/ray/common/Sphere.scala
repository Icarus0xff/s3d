package ray.common

import com.badlogic.gdx.math.collision.Ray
import com.badlogic.gdx.math.{Intersector, Vector3}
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Material.Material
import ray.common.Surface.Surface


case class Sphere(center: Vector3D, radius: Double, color: Vector3D, surface: Surface = Surface.REGULAR, material: Material = Material.AIR) extends Object3D{

  val radius2: Double = radius * radius


  implicit def myToVector3(s: Vector3D): Vector3 = {
    new Vector3(s.getX.toFloat, s.getY.toFloat, s.getZ.toFloat)
  }

  implicit def vector3ToMy(s: Vector3): Vector3D = {
    new Vector3D(s.x, s.y, s.z)
  }

  override def intersect(o: Vector3D, dir: Vector3D): (Boolean, Double) = {
    val ip = new Vector3()
    val ray = new Ray(o, dir)
    val rs = Intersector.intersectRaySphere(ray, center, radius.toFloat, ip)

    (rs, ip distance o)
  }

  override def normal(p: Vector3D): Vector3D = {
    (p subtract center).normalize()
  }

}
