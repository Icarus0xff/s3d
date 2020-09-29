package ray.algo

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import ray.common.MyRay.RayIntersection
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
      rayIntersections: Array[RayIntersection] = MyRay.rayIntersections(eye, randomDir, scene.sceneObjs)
      ri = MyRay.seekNearestObj(rayIntersections)
    } yield {
      val phit = eye add (randomDir scalarMultiply ri.distance)

      shade(ri, phit, randomDir) scalarMultiply (1f / maxRandomRay)
    }

    import Utils._
    val color: Vector3D = colorPartial.reduce { (l, r) => l add r }

    color.getRGB
  }

  private def shade(ri: RayIntersection, phit: Vector3D, dir: Vector3D): Vector3D = {

    val uniformPointOnLight = light.uniformSample()
    val tdir = (phit subtract uniformPointOnLight) normalize
    val bias = tdir scalarMultiply 16
    val aPoint = uniformPointOnLight add bias
    val rayIntersections: Array[RayIntersection] = MyRay.intersect(aPoint, tdir, Set(ri.obj)).toArray


    val cosTheta$ = tdir.dotProduct(light.normal(uniformPointOnLight))

    val ppr = 0.7f

    val c: Vector3D = cosTheta$ match {
      case v if v > 0 && rayIntersections.size > 0 =>
        val cosTheta = FastMath.max(-tdir.dotProduct(ri.obj.normal(phit)), 0)

        val fr = ri.obj.color
        val liMultiFr = Utils.multi(fr scalarMultiply 255, light.color scalarMultiply 255) scalarMultiply cosTheta scalarMultiply cosTheta$
        val dirLight = liMultiFr scalarMultiply (1f / (uniformPointOnLight distanceSq phit)) scalarMultiply (light.surfaceArea / 32)


        val indirLight = Random.nextDouble() match {
          case v if v > ppr => Vector3D.ZERO
          case _ =>
            val u = Random.nextDouble()
            val v = Random.nextDouble()
            val rt = Utils.unifromSampleHemisphere(u, v)
            val costhetaaa = rt dotProduct ri.obj.normal(phit)
            val randomDir = costhetaaa match {
              case v if v > 0 => rt
              case _ => rt negate
            }


            val cos = BSDFUtils.allCollision(phit, randomDir, scene.sceneObjs diff Set(ri.obj)).toArray
            cos.size match {
              case 0 => Vector3D.ZERO
              case _ =>
                val hitObj = MyRay.seekNearestObj(cos)

                val q = phit add (randomDir scalarMultiply hitObj.distance)
                import ray.common.Utils._
                Utils.multi(shade(hitObj, q, randomDir),
                  Phong.renderPix(phit, randomDir, hitObj.distance, light, hitObj.obj, .5, .5, hitObj.obj.color)
                    .scalarMultiply(.02 * 1 / FastMath.PI)
                ) scalarMultiply FastMath.abs(costhetaaa) scalarMultiply 1 / ppr
            }


        }


        val r = dirLight add indirLight
        //println(s"in: $indirLight")
        r
      case _ => Vector3D.ZERO
    }

    c


  }

}
