package vine.geometry.geometry2

import vine.geometry._

/**
 * A directed line segment in a Euclidean plane.
 */
trait Line {

  def a: Vec
  def b: Vec
  def ab: Vec
  def mag: Float
  def ang: Float
  def side(p: Vec): Side = if (p.-(a).cross(b.-(a)) > 0) LeftSide else RightSide
  def add(offset: Vec): Line
  def sub(offset: Vec): Line
  def midpoint: Vec
  def bisect: Line = pointAndStep(midpoint, angleVecUnit(ang).rot90)

  /** Not equal, but correlated, to "bulge" as defined by Jarek Rossignac. */
  def bulge(p: Vec): Float = {
    val c: Circle = circumscribeTriangle(a, b, p)
    c.radius * side(p).i * side(c.center).i
  }

}

private[geometry2] class AtoB (val _a: Vec, val _b: Vec) extends Line {

  var _ab: Option[Vec] = None

  def this(a: Vec, b: Vec, ab: Option[Vec]) { this(a, b); _ab = ab }

  override def a = _a
  override def b = _b
  override def ab = { if (_ab.isEmpty) { _ab = Some(b.-(a)) }; _ab.get }
  override def ang = ab.ang
  override def mag = ab.mag
  override def add(offset: Vec) = new AtoB(offset.+(a), b.+(offset), _ab)
  override def sub(offset: Vec) = new AtoB(offset.-(a), b.-(offset), _ab)
  override def midpoint: Vec = a.+(b)./(2)
  override def toString = "Line %s to %s".format(a, b)

}

private[geometry2] class PointAndDirection (val _a: Vec, val _ab: Vec) extends Line {

  override def a = _a
  override def b = a.+(ab)
  override def ab = _ab
  override def ang = ab.ang
  override def mag = ab.mag
  override def add(offset: Vec) = pointAndStep(a + offset, ab)
  override def sub(offset: Vec) = pointAndStep(a - offset, ab)
  override def midpoint = ab./(2).+(a)

}

private[geometry2] class OriginLine (val _b: Vec) extends Line {

  override def a = origin
  override def b = _b
  override def ab = b
  override def ang = b.ang
  override def mag = b.mag
  override def add(offset: Vec) = aToB(offset, b.+(offset))
  override def sub(offset: Vec) = aToB(offset.*(-1), b.-(offset))
  override def midpoint = b./(2)

}
