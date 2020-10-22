package ray.scenes

import java.awt.Color

import com.badlogic.gdx.math.Plane
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils._
import ray.common._

object LightDraw{


  val big = Sphere(new Vector3D(1600, 1000, 600f), 200f, new Vector3D(.9, .5, 0), surface = Surface.REGULAR,
    materialEta = MaterialEta.GLASS)

  val middle = Sphere(new Vector3D(600, 990, 700f), 256f, new Vector3D(.6, .5, .9), surface = Surface.REGULAR,
    materialEta = MaterialEta.GLASS)

  val small = Sphere(new Vector3D(1000, 1100, 100f), 150f, new Vector3D(.5, .1, 0), surface = Surface.REFRACTIVE,
    materialEta = MaterialEta.GLASS)


  val light = Sphere(new Vector3D(800, -600, 700f), 600, new Vector3D(1, 1, 1))


  val planeLeft = MyPlane(plane = new Plane(new Vector3D(1, 0, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.5, .5, .5))
  val planeRight = MyPlane(plane = new Plane(new Vector3D(-1, 0, 0), new Vector3D(2400, 0, 0)), color = new Vector3D(.9, .9, 0))
  val planeCeil = MyPlane(plane = new Plane(new Vector3D(0, 1, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.5, .5, .5))
  val planeBack = MyPlane(plane = new Plane(new Vector3D(0, 0, 1), new Vector3D(0, 0, -2400)), color = new Vector3D(.5, .5, .5))

  val triangle = Triangle(
    new Vector3D(0, -800, 1700), //a
    new Vector3D(0, 1200, 1700), //b
    new Vector3D(2400, 1200, 1700), //c
    color = new Vector3D(.5, .5, .5)
  )
  val triangle1 = Triangle(
    new Vector3D(0, -800, 1700), //a
    new Vector3D(2400, 1200, 1700), //c
    new Vector3D(2400, -800, 1700), //b
    color = new Vector3D(0, .5, .5)
  )

  val floor = Triangle(
    new Vector3D(0, 1200, -1000), //a
    new Vector3D(2400, 1200, 1700), //b
    new Vector3D(0, 1200, 1700), //c
    color = new Vector3D(.5, .5, .5),
    //surface = Surface.REFLECTIVE
  )

  val floor1 = Triangle(
    new Vector3D(0, 1200, -1000), //a
    new Vector3D(2400, 1200, 100), //b
    new Vector3D(2400, 1200, 1700), //b
    color = new Vector3D(.5, .5, .5),
    //surface = Surface.REFLECTIVE
  )


  val sceneObjs: Set[Object3D] = Set(triangle1, triangle, planeLeft, planeRight, planeCeil, floor, floor1, big, middle, small, planeBack, light)

}
