package ray.scenes

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.{Object3D, Sphere, Surface, Triangle}

object Scene{
  val smallSphere = Sphere(new Vector3D(700, 600, 1000f), 300f, new Vector3D(.25, .4, 0))
  val sphere1 = Sphere(new Vector3D(1600, 900, 800f), 256f, new Vector3D(.5, .5, .5))

  val sphereRefraction = Sphere(new Vector3D(1700, 100, 900f), 200f, new Vector3D(.3, .1, .5))


  val light = Sphere(new Vector3D(900, 100, 1000f), 80, new Vector3D(.25, .45, .07))


  val triangle = Triangle(
    new Vector3D(0, -1200, 1700), //a
    new Vector3D(0, 1200, 1700), //b
    new Vector3D(2400, 1200, 1700), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangleBack = Triangle(
    new Vector3D(0, -1200, -1000), //a
    new Vector3D(0, 1200, -1000), //b
    new Vector3D(2400, 1200, -1000), //c
    color = new Vector3D(.6, 0, .6)
  )

  val triangle1 = Triangle(
    new Vector3D(0, -1200, 1700), //a
    new Vector3D(2400, 1200, 1700), //c
    new Vector3D(2400, -1200, 1700), //b
    color = new Vector3D(0, .4, 0)
  )

  val triangleLeft = Triangle(
    new Vector3D(0, -1200, 1700), //a
    new Vector3D(0, 1200, 100), //a
    new Vector3D(0, 1200, 1700), //a
    color = new Vector3D(0, 0, .6),
    //reflective = true
  )

  val triangleRight = Triangle(
    new Vector3D(2400, -1200, 1700), //a
    new Vector3D(2400, 1200, 1700), //a
    new Vector3D(2400, 1200, 100), //a
    color = new Vector3D(0, .3, .4),
    //reflective = true
  )

  val floor = Triangle(
    new Vector3D(0, 1200, 100), //a
    new Vector3D(2400, 1200, 1700), //b
    new Vector3D(0, 1200, 1700), //c
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )

  val floor1 = Triangle(
    new Vector3D(0, 1200, 100), //a
    new Vector3D(2400, 1200, 100), //b
    new Vector3D(2400, 1200, 1700), //b
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )

  val scene: Set[Object3D] = Set(triangle1, triangle, triangleLeft, triangleRight, floor, floor1, sphere1, smallSphere, sphereRefraction)

}
