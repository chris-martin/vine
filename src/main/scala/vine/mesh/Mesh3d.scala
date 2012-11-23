package vine.mesh

import vine.geometry.geometry3._

class Mesh3d extends Mesh[Vec] {

  def shift(offset: Vec) { for (v <- vertices) v.location = v.location + offset }

  def translateCenterToOrigin() { shift(center * -1) }

  def center = {
    var min = xyz(Float.MaxValue, Float.MaxValue, Float.MaxValue)
    var max = xyz(Float.MinValue, Float.MinValue, Float.MinValue)
    for (v <- vertices) {
      min = xyz(math.min(min.x, v.location.x), math.min(min.y, v.location.y), math.min(min.z, v.location.z))
      max = xyz(math.max(max.x, v.location.x), math.max(max.y, v.location.y), math.max(max.z, v.location.z))
    }
    midpoint(min, max)
  }

}
