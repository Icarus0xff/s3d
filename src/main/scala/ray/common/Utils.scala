package ray.common

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Utils{

  implicit def aTob(s: Vector3D): Vec3f = Vec3f(s.getX, s.getY, s.getZ)

  case class Vec3f(x: Double, y: Double, z: Double) extends Vector3D(x, y, z){


    def -(that: Vec3f) = Vec3f(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Vec3f) = Vec3f(this.x + that.x, this.y + that.y, this.z + that.z)

    def +(that: Double) = Vec3f(this.x + that, this.y + that, this.z + that)

    def vecMultiply(that: Vec3f) = Vec3f(this.x * that.x, this.y * that.y, this.z * that.z)

    def *(that: Double) = Vec3f(x * that, y * that, z * that)

    def /(that: Double) = Vec3f(x / that, y / that, z / that)

    def dot(that: Vec3f) = this.x * that.x + this.y * that.y + this.z * that.z

    def length() = Math.sqrt(x * x + y * y + z * z)


    def norm() = {
      val nor2 = x * x + y * y + z * z

      val normed = nor2 match {
        case _ if nor2 > 0 =>
          val invNor = 1 / Math.sqrt(nor2)
          Vec3f(x * invNor, y * invNor, z * invNor)
        case _ => this
      }

      normed
    }
  }

  case class Triangle(A: Vec3f, B: Vec3f, C: Vec3f, color: Vector3D) extends Object3D{
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

  case class Sphere(center: Vec3f, radius: Double, color: Vector3D) extends Object3D{

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
        case (a, b) if a > b =>
          (true, a)
        case (a, b) if a < b =>
          (true, b)
      }
    }

    override def normal(p: Vec3f): Vec3f = {
      (p - center).norm
    }

  }

}
