package ray.scenes

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils.Vec3f
import ray.common.{Object3D, Sphere, Surface, Triangle}

object Scene{
  val smallSphere = Sphere(Vec3f(700, 600, 1000f), 300f, new Vector3D(.25, .4, 0))
  val sphere1 = Sphere(Vec3f(1600, 900, 800f), 256f, new Vector3D(.5, .5, .5))

  val sphereRefraction = Sphere(Vec3f(1700, 100, 900f), 200f, new Vector3D(.3, .1, .5))


  val light = Sphere(Vec3f(900, 100, 1000f), 80, new Vector3D(.25, .45, .07))


  val triangle = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 1700), //b
    Vec3f(2400, 1200, 1700), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangleBack = Triangle(
    Vec3f(0, -1200, -1000), //a
    Vec3f(0, 1200, -1000), //b
    Vec3f(2400, 1200, -1000), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangle1 = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(2400, 1200, 1700), //c
    Vec3f(2400, -1200, 1700), //b
    color = new Vector3D(0, .4, 0)
  )

  val triangleLeft = Triangle(
    Vec3f(0, -1200, 1700), //a
    Vec3f(0, 1200, 100), //a
    Vec3f(0, 1200, 1700), //a
    color = new Vector3D(0, 0, .6),
    //reflective = true
  )

  val triangleRight = Triangle(
    Vec3f(2400, -1200, 1700), //a
    Vec3f(2400, 1200, 1700), //a
    Vec3f(2400, 1200, 100), //a
    color = new Vector3D(0, .3, .4),
    //reflective = true
  )

  val floor = Triangle(
    Vec3f(0, 1200, 100), //a
    Vec3f(2400, 1200, 1700), //b
    Vec3f(0, 1200, 1700), //c
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )

  val floor1 = Triangle(
    Vec3f(0, 1200, 100), //a
    Vec3f(2400, 1200, 100), //b
    Vec3f(2400, 1200, 1700), //b
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )

  val scene: Set[Object3D] = Set(triangle1, triangle, triangleLeft, triangleRight, floor, floor1, sphere1, smallSphere, sphereRefraction)

}
