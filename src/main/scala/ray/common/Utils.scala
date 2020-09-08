package ray.common

object Utils{

  case class Vec3f(x: Double, y: Double, z: Double){

    def -(that: Vec3f) = Vec3f(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Vec3f) = Vec3f(this.x + that.x, this.y + that.y, this.z + that.z)

    def +(that: Double) = Vec3f(this.x + that, this.y + that, this.z + that)

    def *(that: Vec3f) = Vec3f(this.x * that.x, this.y * that.y, this.z * that.z)

    def *(that: Double) = Vec3f(x * that, y * that, z * that)

    def dot(that: Vec3f) = this.x * that.x + this.y * that.y + this.z * that.z


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

  case class Plane(n: Vec3f, d: Double) extends Object3D{
    override def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double) = {
      val a = d - (n dot o)
      val b = n dot dir

      b match {
        case 0 => (false, 0)
        case _ => (true, a / b)
      }

    }

    override def normal(p: Vec3f): Vec3f = {
      n
    }
  }

  case class Sphere(center: Vec3f, radius: Double) extends Object3D{

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
