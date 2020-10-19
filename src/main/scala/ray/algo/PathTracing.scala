package ray.algo

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.FastMath
import org.apache.log4j.{LogManager, Logger}
import ray.common.MyRay.RayIntersection
import ray.common._
import ray.scenes.LightDraw




object PathTracing{
  val scene = LightDraw
  val light = scene.light
  val rn = new scala.util.Random(System.nanoTime())
  val rn1 = new scala.util.Random(System.nanoTime() - 1997)

  lazy val logger: Logger = LogManager.getLogger(getClass.getName.stripSuffix("$"))


  def renderPix(xi: Int, yi: Int, maxRandomRay: Int, width: Int, height: Int): Int = {
    val eye = new Vector3D(width / 2, height / 2, -700f)

    val colorPartial = for {
      _ <- 1 to maxRandomRay
      r = rn.nextDouble()
      xx = xi + r
      yy = yi + r
      randomDir: Vector3D = (new Vector3D(xx, yy, 0) subtract eye) normalize()
      rayIntersections: Array[RayIntersection] = MyRay.rayIntersections(eye, randomDir, scene.sceneObjs)
      ri = MyRay.seekNearestObj(rayIntersections)
    } yield {
      val phit = eye add (randomDir scalarMultiply ri.distance)

      val c = shade(ri, phit) scalarMultiply (1f / maxRandomRay) scalarMultiply 2 * FastMath.PI
      c
    }

    import Utils._
    val color: Vector3D = colorPartial.reduce { (l, r) => l add r }

    color.getRGB
  }

  import ray.common.Utils._

  private def shade(ri: RayIntersection, phit: Vector3D) = {
    lazy val dirLight: Vector3D = directLight(ri, phit)


    /**
      * 非直接光照部分的计算。
      * 1. phit点朝着light随机发射一根光线（uniform hemisphere sample）
      * 2. 计算这根光线的反射（折射）方向，然后递归追踪，如果这条光线既有折射也有反射，那么根据fresnel概率性选择一个方向。
      * 3. 然后加总计算
      */
    lazy val indr = indirLight(ri, phit)

    indr


  }


  private def indirLight(ri: RayIntersection, phit: Vector3D): Vector3D = {
    ri.obj match {
      case LightDraw.light =>
        return ri.obj.color scalarMultiply 255
      case _ =>
    }
    val ppr = .9f

    ri.obj.surface match {
      case ray.common.Surface.REFRACTIVE =>
        rn.nextDouble() match {
          case v if v > ppr => Vector3D.ZERO
          case _ =>
            val randomDir = BSDFUtils.reflect(ri.originDir, ri.obj.normal(phit))

            val collisions = BSDFUtils.allCollision(phit, randomDir, scene.sceneObjs diff Set(ri.obj)).toArray
            collisions.size match {
              case 0 => Color.BLACK
              case _ =>
                val hitObj = MyRay.seekNearestObj(collisions)
                if (hitObj.obj.eq(light)) {
                  return ri.obj.color scalarMultiply 255
                }
                val pphit = (hitObj.originDir scalarMultiply hitObj.distance) add phit
                val ei = indirLight(hitObj, pphit)

                ei scalarMultiply (.4 * 1 / ppr)
            }
        }
      case ray.common.Surface.REFLECTIVE =>
        rn.nextDouble() match {
          case v if v > ppr => Vector3D.ZERO
          case _ =>
            val randomDir = BSDFUtils.reflect(ri.originDir, ri.obj.normal(phit))

            val collisions = BSDFUtils.allCollision(phit, randomDir, scene.sceneObjs diff Set(ri.obj)).toArray
            collisions.size match {
              case 0 => Color.BLACK
              case _ =>
                val hitObj = MyRay.seekNearestObj(collisions)
                if (hitObj.obj.eq(light)) {
                  return ri.obj.color scalarMultiply 255
                }
                val pphit = (hitObj.originDir scalarMultiply hitObj.distance) add phit
                val ei = indirLight(hitObj, pphit)

                ei scalarMultiply (.4 * 1 / ppr)
            }
        }
      case ray.common.Surface.REGULAR =>
        rn.nextDouble() match {
          case v if v > ppr => Vector3D.ZERO
          case _ =>
            val u = rn.nextDouble()
            val v = rn1.nextDouble()
            val uniformUnit = Utils.unifromSampleHemisphere(u, v)
            val cosΘ = uniformUnit dotProduct ri.obj.normal(phit)
            /**
              * cosΘ < 90 degree
              */
            val randomDir = cosΘ match {
              case v if v > 0 => uniformUnit
              case _ => uniformUnit negate
            }


            val collisions = BSDFUtils.allCollision(phit, randomDir, scene.sceneObjs diff Set(ri.obj)).toArray
            collisions.size match {
              case 0 => Color.BLACK
              case _ =>
                val hitObj = MyRay.seekNearestObj(collisions)
                if (hitObj.obj.eq(light)) {
                  return ri.obj.color scalarMultiply 255
                }
                val pphit = (hitObj.originDir scalarMultiply hitObj.distance) add phit
                val ei = indirLight(hitObj, pphit) scalarMultiply FastMath.abs(cosΘ)
                val brdf =.4 / FastMath.PI

                ei scalarMultiply (FastMath.PI * 2f * brdf * 1 / ppr)
            }
        }
    }
  }

  private def directLight(ri: RayIntersection, phit: Vector3D): Vector3D = {
    val uniformPointOnLight = light.uniformSample()
    val tdir = (phit subtract uniformPointOnLight) normalize
    val bias = tdir scalarMultiply 1.1
    val aPoint = uniformPointOnLight add bias

    val objsExceptRi = scene.sceneObjs diff Set(ri.obj)
    val maxDistance = light.center.distance(phit)
    val notIntersectedObjs = objsExceptRi.takeWhile { object3D =>
      val r = object3D.intersect(aPoint, tdir)
      r._1 == false || r._2 > maxDistance
    }

    val cosTheta$ = tdir.dotProduct(light.normal(uniformPointOnLight))


    val dirLight: Vector3D = cosTheta$ match {
      case v if v > 0 && objsExceptRi.size - notIntersectedObjs.size == 0 =>
        val cosTheta = FastMath.max(-tdir.dotProduct(ri.obj.normal(phit)), 0)

        val fr = ri.obj.color
        val liMultiFr = Utils.multi(fr scalarMultiply 255, light.color scalarMultiply 255) scalarMultiply cosTheta scalarMultiply cosTheta$
        liMultiFr scalarMultiply (1f / (uniformPointOnLight distanceSq phit)) scalarMultiply (light.surfaceArea / 2048)
      case _ => Vector3D.ZERO
    }
    dirLight
  }
}
