package ray.common

object MaterialEta extends Enumeration{

  type MaterialEta = Val

  protected case class Val(eta: Double) extends super.Val

  val AIR = Val(1.0)
  val GLASS = Val(1.52)
}
