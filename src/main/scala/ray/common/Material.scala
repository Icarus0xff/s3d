package ray.common

object Material extends Enumeration{

  type Material = Val

  protected case class Val(eta: Double) extends super.Val

  val NONE = Val(-1f)
  val AIR = Val(1.0)
  val GLASS = Val(1.52)
}
