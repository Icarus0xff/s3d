package ray.common

import com.badlogic.gdx.math.collision.Ray
import com.badlogic.gdx.math.{Intersector, Plane, Vector3}
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.MaterialEta.MaterialEta
import ray.common.Surface.Surface
import ray.common.Utils._


case class MyPlane(plane: Plane, color: Vector3D, surface: Surface = Surface.REGULAR, materialEta: MaterialEta = MaterialEta.AIR) extends Object3D{

  override def intersect(o: Vector3D, dir: Vector3D): (Boolean, Double) = {
    val ip = new Vector3()
    val ray = new Ray(o, dir)

    val r = Intersector.intersectRayPlane(ray, plane, ip)

    (r, ip distance o)
  }

  override def normal(p: Vector3D): Vector3D = {
    plane.normal
  }

  override def surfaceArea(): Double = ???

  override def uniformSample(): Vector3D = ???

}
