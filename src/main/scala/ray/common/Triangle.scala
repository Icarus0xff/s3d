package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils.Vec3f

case class Triangle(A: Vec3f, B: Vec3f, C: Vec3f, color: Vector3D, reflective: Boolean = false) extends Object3D{
  private val AB = B subtract A
  private val AC = C subtract A
  private val na = AB crossProduct AC
  private val nb = 1 / (na getNorm)

  val n = na scalarMultiply (nb)


  override def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double) = {
    val d = n dotProduct A

    val ta = d - (n dotProduct o)
    val tb = n dotProduct dir
    val t = ta / tb


    tb match {
      case m if Math.abs(m - 0) < 0.000001 =>
        (false, 0)
      case _ => {
        val Q = o add (dir scalarMultiply t)

        def test(b1: Vec3f, a1: Vec3f) = {
          ((b1 subtract a1) crossProduct (Q subtract a1)) dotProduct n match {
            case m if Math.abs(m - 0) < 0.0001 || m > 0 =>
              true
            case _ =>
              false
          }
        }

        test(B, A) && test(C, B) && test(A, C) && t > 0 match {
          case true => (true, t)
          case _ => (false, 0)
        }
      }
    }

  }

  override def normal(p: Vec3f): Vec3f = Vec3f(n.getX, n.getY, n.getZ)
}



