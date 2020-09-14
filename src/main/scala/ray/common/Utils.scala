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




}
