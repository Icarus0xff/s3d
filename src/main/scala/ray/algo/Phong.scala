package ray.algo

import java.awt.Color

import ray.common.Utils.{Sphere, Vec3f}

object Phong{
  def renderPix(eye: Vec3f, eyeToPix: Vec3f, d: Double, light: Sphere, sphere: Sphere): Color = {
    val pHit = eye + (eyeToPix * d)

    val ambientStrength = .2
    val lightColor = Vec3f(255, 255, 255)
    val ambient = lightColor * ambientStrength

    val norm = (pHit - sphere.center).norm
    val lightDir = (light.center - pHit) norm
    val diff = Math.max(lightDir dot norm, 0)
    val diffuse = lightColor * diff

    val specularStrength = 0.8

    val viewDir = (pHit - eye) norm
    val reflectDir = norm * (2 * (lightDir dot norm)) - lightDir

    val spec = Math.pow(Math.max(viewDir dot reflectDir, 0.0), 32)
    val specular = lightColor * (specularStrength * spec)

    val result = (ambient + diffuse + specular) * Vec3f(.25, .45, .07)

    new Color(result.x.toInt, result.y.toInt, result.z.toInt)
  }
}
