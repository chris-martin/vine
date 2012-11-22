package vine.geometry.geometry3

import vine.geometry._

trait Circle {

  def center: Vec

  /** A vector orthogonal to the plane on which the circle lies. */
  def normal: Vec

  def radius: Float

  /** This sphere's intersection with the XY plane. 0 or 1 circles. */
  def intersectXY: Option[geometry2.Circle]

}

class SimpleCircle
    ( val _center: Vec, val _normal: Vec, val _radius: Float ) extends Circle {

  override def center = _center
  override def normal = _normal
  override def radius = _radius

  override def intersectXY = {
    val r: Float = radius
    val d: Float = center.z
    if (d > r) None
    else Some(geometry2.circle(center.xy, r * r - d * d))
  }

}
