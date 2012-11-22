package vine.geometry.geometry3

/**
 * A directed line segment in three dimensional space.
 */
trait Line {

  def a: Vec
  def b: Vec
  def ab: Vec
  def mag: Float = ab.mag
  def +(offset: Vec): Line
  def -(offset: Vec): Line
  def midpoint: Vec

  /** Changes A without affecting B. */
  def a(a: Vec): Line = aToB(a, b)

  /** Changes B without affecting A. */
  def b(b: Vec): Line = aToB(a, b)

  /** Changes A without affecting AB. */
  def aShift(a: Vec): Line = pointAndStep(a, ab)

  /** Changes B without affecting AB. */
  def bShift(b: Vec): Line = new AtoB(b - ab, b, Some(ab))

  /** Changes AB without affecting A. */
  def ab(ab: Vec): Line = pointAndStep(a, ab)

  /** Elevation of AB. */
  def elevation: Float = ab.elevation

  /** Azimuth of AB. */
  def azimuth: Float = ab.azimuth

  /** An arbitrary line, passing through A, orthogonal to this line. */
  def aOrthog: Line = pointAndStep(a, ab.orthog)

  /** An arbitrary line, passing through B, orthogonal to this line. */
  def bOrthog: Line = reverse.aOrthog

  def reverse: Line

  /** Move b such that the magnitude of ab is multiplied. */
  def mult(factor: Float): Line = pointAndStep(a, ab.*(factor))

  override def toString = "%s -> %s".format(a, b)

}

private[geometry3] class AtoB (val _a: Vec, val _b: Vec) extends Line {

  def this(a: Vec, b: Vec, ab: Option[Vec]) {
    this(a, b)
    _ab = ab
  }

  var _ab: Option[Vec] = None

  override def ab = {
    if (_ab.isEmpty) { _ab = Some(b.-(a)) }
    _ab.get
  }

  override def a = _a
  override def b = _b
  override def +(offset: Vec) = new AtoB(offset.+(a), b.+(offset), _ab)
  override def -(offset: Vec) = new AtoB(a.-(offset), b.-(offset), _ab)
  override def reverse = new AtoB(b, a, _ab.map($ => $.*(-1)))
  override def midpoint = a.+(b)./(2)
  override def toString = "Line %s to %s".format(a, b)

}

private[geometry3] class PointAndDirection (val _a: Vec, val _ab: Vec) extends Line {

  override def a: Vec = _a
  override def b: Vec = a + ab
  override def ab: Vec = _ab
  override def +(offset: Vec) = new PointAndDirection(a + offset, ab)
  override def -(offset: Vec) = new PointAndDirection(a - offset, ab)
  override def reverse = new PointAndDirection(b, ab * -1)
  override def midpoint = a + (ab / 2)

}
