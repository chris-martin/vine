package vine.geometry.geometry2

trait Circle {
  def center: Vec
  def radius: Float
}

private[geometry2] class SimpleCircle (val _center: Vec, val _radius: Float) extends Circle {

  override def center = _center
  override def radius = _radius

}

private[geometry2] class TriangleCircle2 (val vs: Array[Vec]) extends Circle {

  var _center: Option[Vec] = None
  var _radius: Option[Float] = None

  override def center: Vec = {
    if (_center.isEmpty) {
      _center = Some(intersectLines(
        aToB(vs(0), vs(1)).bisect,
        aToB(vs(1), vs(2)).bisect
      ))
    }
    _center.get
  }

  override def radius: Float = {
    if (_radius.isEmpty) {
      _radius = Some(center.-(vs(0)).mag)
    }
    _radius.get
  }

}