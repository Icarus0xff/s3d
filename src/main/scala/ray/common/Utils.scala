package ray.common

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Utils{

  implicit def vector3DToVec3f(s: Vector3D): Vec3f = Vec3f(s.getX, s.getY, s.getZ)

  implicit def vector3DToColor(s: Vector3D): Color = new Color(s.getX toInt, s.getY toInt, s.getZ toInt)

  implicit def colorToVector3D(s: Color): Vector3D = new Vector3D(s.getRed, s.getGreen, s.getBlue)

  case class Vec3f(x: Double, y: Double, z: Double) extends Vector3D(x, y, z){


    def -(that: Vec3f) = this subtract that

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
