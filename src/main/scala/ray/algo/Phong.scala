package ray.algo

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils.Vec3f
import ray.common.{Object3D, Sphere}

object Phong{
  def renderPix(eye: Vec3f, viewDir: Vec3f, eyeToPDistance: Double, light: Sphere, obj: Object3D, ambientStrength: Double, specularStrength: Double, colornew: Vector3D): Color = {
    val pHit = eye + (viewDir * eyeToPDistance)

    val lightColor = Vec3f(192, 192, 192)
    val ambient = lightColor scalarMultiply ambientStrength

    val norm = obj.normal(pHit)
    val lightDir = (light.center - pHit) normalize
    val cosTheta = Math.max(lightDir dotProduct norm, 0)
    val diffuse = lightColor scalarMultiply cosTheta

    val reflectDir = (norm scalarMultiply (2 * (norm dotProduct lightDir))) subtract lightDir


    val specular$ = Math.pow(Math.max(viewDir.negate dotProduct reflectDir, 0.0), 32)
    val specular = lightColor scalarMultiply (specularStrength * specular$)


    val result1 = ambient add diffuse add specular
    val result = Vec3f(result1 getX, result1 getY, result1 getZ) vecMultiply
      colornew


    new Color(result.x.toInt, result.y.toInt, result.z.toInt)
  }
}
