package ray.common

import ray.common.Utils.Vec3f

trait Object3D{
  def normal(p: Vec3f): Vec3f

  def intersect(o: Vec3f, dir: Vec3f): (Boolean, Double)
}
