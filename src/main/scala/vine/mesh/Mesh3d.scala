package vine.mesh

import vine.geometry.geometry3._

class Mesh3d extends Mesh {

  override type Location = Vec

  def shift(offset: Vec) {
    for (v <- vertices) v.location += offset
  }

  def translateCenterToOrigin() {
    shift(center * -1)
  }

  def center = {

    import math.{min, max}

    var a = xyzSeq(List.fill(3)(Float.MaxValue))
    var b = xyzSeq(List.fill(3)(Float.MinValue))

    for (v <- vertices) {
      val p = v.location
      a = xyz(min(a.x, p.x), min(a.y, p.y), min(a.z, p.z))
      b = xyz(max(b.x, p.x), max(b.y, p.y), max(b.z, p.z))
    }

    midpoint(a, b)
  }

}
