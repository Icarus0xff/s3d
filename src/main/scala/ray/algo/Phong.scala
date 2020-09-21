package ray.algo

import java.awt.Color

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.{Object3D, Sphere}

object Phong{
  def renderPix(eye: Vector3D, viewDir: Vector3D, eyeToPDistance: Double, light: Sphere, obj: Object3D, ambientStrength: Double, specularStrength: Double, colornew: Vector3D): Color = {
    val pHit = eye add (viewDir scalarMultiply eyeToPDistance)

    val lightColor = new Vector3D(200, 200, 200)
    val ambient = lightColor scalarMultiply ambientStrength

    val norm = obj.normal(pHit)
    val lightDir = (light.center subtract pHit) normalize
    val cosTheta = Math.max(lightDir dotProduct norm, 0)
    val diffuse = lightColor scalarMultiply cosTheta

    val reflectDir = (norm scalarMultiply (2 * (norm dotProduct lightDir))) subtract lightDir


    val specular$ = Math.pow(Math.max(viewDir.negate dotProduct reflectDir, 0.0), 32)
    val specular = lightColor scalarMultiply (specularStrength * specular$)


    val result1 = ambient add diffuse add specular
    val result = new Vector3D(
      result1.getX * colornew.getX,
      result1.getY * colornew.getY,
      result1.getZ * colornew.getZ,
    )


    import ray.common.Utils._
    result
  }
}
