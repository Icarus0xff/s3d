package ray.scenes

import com.badlogic.gdx.math.Plane
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import ray.common.Utils._
import ray.common._

object LightDraw{


  val sphereRefraction = Sphere(new Vector3D(1400, 940, 500f), 300f, new Vector3D(.5, .5, 0), surface = Surface.REGULAR,
    materialEta = MaterialEta.GLASS)

  val sphereRefraction1 = Sphere(new Vector3D(600, 940, 700f), 256f, new Vector3D(0, 0, .5), surface = Surface.REGULAR,
    materialEta = MaterialEta.GLASS)


  val light = Sphere(new Vector3D(1400, 0, 600f), 200, new Vector3D(1, 1, 1))


  val planeLeft = MyPlane(plane = new Plane(new Vector3D(1, 0, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.5, .5, .5))
  val planeRight = MyPlane(plane = new Plane(new Vector3D(-1, 0, 0), new Vector3D(2400, 0, 0)), color = new Vector3D(0, .5, 0))
  val planeCeil = MyPlane(plane = new Plane(new Vector3D(0, 1, 0), new Vector3D(0, -800, 0)), color = new Vector3D(.5, .5, .5))

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
    color = new Vector3D(.5, .5, .5)
  )

  val floor1 = Triangle(
    new Vector3D(0, 1200, -1000), //a
    new Vector3D(2400, 1200, 100), //b
    new Vector3D(2400, 1200, 1700), //b
    color = new Vector3D(.5, .5, .5),
  )


  val sceneObjs: Set[Object3D] = Set(triangle1, triangle, planeLeft, planeRight, planeCeil, floor, floor1, sphereRefraction, sphereRefraction1, light)

}
