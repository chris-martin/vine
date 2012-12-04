package vine.mesh

import vine.geometry.geometry3, geometry3._

class Mesh3d extends Mesh {

  override type Location = Vec

  def translate(offset: Vec) {
    for (v <- vertices) v.location += offset
  }

  def translateCenterToOrigin() {
    translate(center * -1)
  }

  def center = {
    val bb = boundingBox
    midpoint(bb._1, bb._2)
  }

  def boundingBox: Pair[Vec, Vec] = geometry3.boundingBox(vertices map {_.location})

}