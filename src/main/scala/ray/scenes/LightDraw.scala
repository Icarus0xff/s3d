package ray.scenes

import com.badlogic.gdx.math.Plane
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils._
import ray.common._

object LightDraw{
  val smallSphere = Sphere(new Vector3D(700, 600, 1400f), 300f, new Vector3D(.1, .5, .5))

  val sphereRefraction = Sphere(new Vector3D(1000, 850, 500f), 256f, new Vector3D(.5, .5, .5), surface = Surface.REFRACTIVE,
    material = Material.GLASS)

  val sphereRefractionRight = Sphere(new Vector3D(1600, 100, 1000f), 200f, new Vector3D(.2, .3, .2), surface = Surface.REFRACTIVE)
  val light = Sphere(new Vector3D(900, 100, 1000f), 80, new Vector3D(.25, .45, .07))


  val planeLeft = MyPlane(plane = new Plane(new Vector3D(1, 0, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.1, .1, .3))
  val planeRight = MyPlane(plane = new Plane(new Vector3D(-1, 0, 0), new Vector3D(2400, 0, 0)), color = new Vector3D(.3, .2, .4))
  val planeCeil = MyPlane(plane = new Plane(new Vector3D(0, -1, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.6, .6, .6))

  val triangle = Triangle(
    new Vector3D(0, -800, 1700), //a
    new Vector3D(0, 1200, 1700), //b
    new Vector3D(2400, 1200, 1700), //c
    color = new Vector3D(.5, .2, .6)
  )
  val triangle1 = Triangle(
    new Vector3D(0, -800, 1700), //a
    new Vector3D(2400, 1200, 1700), //c
    new Vector3D(2400, -800, 1700), //b
    color = new Vector3D(.2, .5, .3)
  )

  val floor = Triangle(
    new Vector3D(0, 1200, -1000), //a
    new Vector3D(2400, 1200, 1700), //b
    new Vector3D(0, 1200, 1700), //c
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )

  val floor1 = Triangle(
    new Vector3D(0, 1200, -1000), //a
    new Vector3D(2400, 1200, 100), //b
    new Vector3D(2400, 1200, 1700), //b
    color = new Vector3D(.5, .5, .02),
    surface = Surface.REFLECTIVE
  )


  val scene: Set[Object3D] = Set(triangle1, triangle, planeLeft, planeRight, planeCeil, floor, floor1, sphereRefraction, smallSphere, sphereRefractionRight)

}
