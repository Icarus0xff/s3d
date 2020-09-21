package ray.common

import com.badlogic.gdx.math.collision.Ray
import com.badlogic.gdx.math.{Intersector, Vector3}
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Surface.Surface
import ray.common.Utils.Vec3f


case class Sphere(center: Vec3f, radius: Double, color: Vector3D, surface: Surface = Surface.REGULAR) extends Object3D{

  val radius2: Double = radius * radius


  implicit def myToVector3(s: Vec3f): Vector3 = {
    new Vector3(s.x.toFloat, s.y.toFloat, s.z.toFloat)
  }

  implicit def vector3ToMy(s: Vector3): Vec3f = {
    Vec3f(s.x, s.y, s.z)
  }

  override def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double) = {
    val ip = new Vector3()
    val ray = new Ray(o, dir)
    val rs = Intersector.intersectRaySphere(ray, center, radius.toFloat, ip)

    (rs, ip distance o)
  }

  override def normal(p: Vec3f): Vec3f = {
    (p - center).normalize()
  }

}
