package ray

import ray.algo.PathTracing
import ray.common.Utils

object App{
  private val width = 1700
  private val height = 1500


  val xi = 0 until width toArray
  val yi = 0 until height toArray
  val MAX_RANDOM_RAY = 128


  def main(args: Array[String]): Unit = {
    val colors = for {
      x <- xi
      y <- yi
    } yield {
      val c = PathTracing.renderPix(x, y, MAX_RANDOM_RAY, width, height)
      (x, y, c)
    }

    Utils.outputImage(width, height, colors)
  }
}
