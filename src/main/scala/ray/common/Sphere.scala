package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils.Vec3f


case class Sphere(center: Vec3f, radius: Double, color: Vector3D, reflective: Boolean = false) extends Object3D{

  val radius2: Double = radius * radius


  override def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double) = {
    val L = center - o

    val tca = L dot dir
    if (tca < 0) {
      return (false, 0)
    }

    val d2 = (L dot L) - tca * tca
    if (d2 > radius2) {
      return (false, 0)
    }

    val thc = Math.sqrt(radius2 - d2);
    val t0 = tca - thc
    val t1 = tca + thc

    (t0, t1) match {
      case (a, b) if a < 0 && b < 0 =>
        (false, 0)
      case (a, b) if a >= b =>
        (true, a)
      case (a, b) if a < b =>
        (true, b)
    }
  }

  override def normal(p: Vec3f): Vec3f = {
    (p - center).normalize()
  }

}
