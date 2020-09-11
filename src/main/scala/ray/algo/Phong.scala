package ray.algo

import java.awt.Color

import ray.common.Object3D
import ray.common.Utils.{Sphere, Vec3f}

object Phong{
  def renderPix(eye: Vec3f, eyeToPix: Vec3f, eyeToPDistance: Double, light: Sphere, obj: Object3D, ambientStrength: Double, specularStrength: Double): Color = {
    val pHit = eye + (eyeToPix * eyeToPDistance)

    val lightColor = Vec3f(255, 255, 255)
    val ambient = lightColor * ambientStrength

    val norm = obj.normal(pHit)
    val lightDir = (light.center - pHit) norm
    val diff = Math.max(lightDir dot norm, 0)
    val diffuse = lightColor * diff

    val viewDir = (pHit - eye) norm
    val reflectDir = norm scalarMultiply (2 * (lightDir dotProduct norm)) subtract lightDir

    val spec = Math.pow(Math.max(viewDir dot reflectDir, 0.0), 32)
    val specular = lightColor * (specularStrength * spec)


    val result1 = ambient add diffuse add specular
    val result = Vec3f(result1 getX, result1 getY, result1 getZ) vecMultiply
      Vec3f(obj.color getX, obj.color getY, obj.color getZ)

    new Color(result.x.toInt, result.y.toInt, result.z.toInt)
  }
}
