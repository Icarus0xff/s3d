package ray.algo

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import ray.common.Ray.RayIntersection
import ray.common._
import ray.scenes.LightDraw

import scala.util.Random


object PathTracing{
  val scene = LightDraw
  val light = scene.light


  def renderPix(xi: Int, yi: Int, maxRandomRay: Int, width: Int, height: Int): Int = {
    val eye = new Vector3D(width / 2, height / 2, -700f)

    val colorPartial = for {
      _ <- 1 to maxRandomRay
      r = Random.nextDouble()
      xx = xi + r
      yy = yi + r
      randomDir: Vector3D = (new Vector3D(xx, yy, 0) subtract eye) normalize()
      rayIntersections: Array[RayIntersection] = Ray.rayIntersections(eye, randomDir, scene.sceneObjs)
      ri = Ray.seekNearestObj(rayIntersections)
    } yield {
      val phit = eye add (randomDir scalarMultiply ri.distance)

      val color: Vector3D = shade(ri, phit, randomDir) scalarMultiply (1f / maxRandomRay)

      color
    }

    import Utils._
    val color: Color = colorPartial.reduce { (l, r) => l add r }
    color.getRGB
  }

  private def shade(ri: RayIntersection, phit: Vector3D, dir: Vector3D): Vector3D = {

    val uniformPointOnLight = light.uniformSample()
    val tdir = (phit subtract uniformPointOnLight) normalize
    val bias = tdir scalarMultiply 16
    val aPoint = uniformPointOnLight add bias
    val rayIntersections: Array[RayIntersection] = Ray.intersect(aPoint, tdir, Set(ri.obj)).toArray


    val cosTheta$ = tdir.dotProduct(light.normal(uniformPointOnLight))

    val c: Vector3D = cosTheta$ match {
      case v if v > 0 && rayIntersections.size > 0 =>
        val cosTheta = FastMath.max(-tdir.dotProduct(ri.obj.normal(phit)), 0)


        val fr = ri.obj.color
        val liMultiFr = Utils.multi(fr scalarMultiply 255, light.color scalarMultiply 255) scalarMultiply cosTheta scalarMultiply cosTheta$
        val lDir = liMultiFr scalarMultiply (1f / (uniformPointOnLight distanceSq phit)) scalarMultiply (light.surfaceArea / 32)


        lDir
      case _ => Vector3D.ZERO
    }

    c


  }
}
