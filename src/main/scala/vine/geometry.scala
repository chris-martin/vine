import math._

package object geometry {

  private val EPSILON: Float = 0.000001f
  private val PI: Float = Pi.toFloat
  private val PI2: Float = PI * 2
  private val HALFPI: Float = PI / 2
  private val NEG_HALFPI: Float = PI / -2
  private val THREE_HALVES_PI: Float = 1.5f * PI

  private def mod2pi(a: Float): Float = {
    if (a < 0) ((a % PI2) + PI2) else (a % PI2)
  }

  private def mod2pi(a: Float, flip: Boolean): Float = {
    mod2pi(if (flip) a + PI else a)
  }

  private def elevation(ang: Float): Float = {
    var a = ang
    var retval: Float = 0
    
    if (a >= NEG_HALFPI && a <= HALFPI) retval = a
    else {
      a = mod2pi(a)
      if (a <= HALFPI) retval = a
      else if (a >= THREE_HALVES_PI) retval = a - PI2
      else retval = PI - a
    }
    
    if (retval >= NEG_HALFPI && retval <= HALFPI) {
      retval
    } else {
      throw new AssertionError(retval)
    }
  }

  private def sign(x: Float): Int = {
    if (x == 0) 0 else (if (x < 0) -1 else 1)
  }

  /**
   * A point in a Euclidean plane.
   */
  trait Vec2 extends Comparable[Vec2] {
    def x: Float
    def y: Float
    def ang: Float
    def mag: Float
    def mag(newMag: Float): Vec2 = angleVec2(ang, newMag)

    /**
     * Equivalent to mag(1).
     */
    def unit: Vec2 = angleVec2(ang, 1)

    def add(o: Vec2): Vec2 = xy(x + o.x, y + o.y)
    def sub(o: Vec2): Vec2 = xy(x - o.x, y - o.y)
    def mult(factor: Float): Vec2
    def mult(factor: Number): Vec2 = mult(factor.floatValue)
    def div(divisor: Float): Vec2
    def div(divisor: Number): Vec2 = div(divisor.floatValue)
    def rot(ang: Float): Vec2 = angleVec2(ang + ang, mag)
    def rot(ang: Number): Vec2 = rot(ang.floatValue)
    def addX($: Float): Vec2 = xy(x + $, y)
    def addY($: Float): Vec2 = xy(x, y + $)
    def subX($: Float): Vec2 = xy(x - $, y)
    def subY($: Float): Vec2 = xy(x, y - $)

    /**
     * This is exactly (0, 0)?
     */
    def isOrigin: Boolean = false

    def rot90: Vec2
    def rot180: Vec2
    def rot270: Vec2

    /**
     * Scalar (dot) product.
     */
    def dot(o: Vec2): Float = x * o.x + y * o.y

    /**
     * U x V = U dot rot90(V).
     */
    def cross(o: Vec2): Float = dot(o.rot90)

    /**
     * Raises this 2d vector into three dimensions by adding a zero-value z value.
     */
    def in3d: Vec3

    def compareTo(o: Vec2): Int = {
      java.lang.Float.compare(mag, o.mag)
    }

    override def toString: String = "(%f, %f)".format(x, y)

  }

  private[geometry] class XY (val _x: Float, val _y: Float) extends Vec2 {

    var _ang: Option[Float] = None
    var _mag: Option[Float] = None

    override def x = _x
    override def y = _y

    override def ang = {
      if (_ang.isEmpty) {
        _ang = Some(math.atan2(y, x).toFloat)
      }
      _ang.get
    }

    override def mag = {
      if (_mag.isEmpty) {
        _mag = Some(math.sqrt(pow(x, 2) + pow(y, 2)).asInstanceOf[Float])
      }
      _mag.get
    }

    override def rot180 = new XY(-1 * x, -1 * y)
    override def rot90 = new XY(-1 * y, x)
    override def rot270 = new XY(y, -1 * x)

    override def mult(f: Float) =
      if (abs(f) < EPSILON) origin2 else xy(f * x, f * y)

    override def div(d: Float): Vec2 = xy(x / d, y / d)

    override def in3d: Vec3 = xyz(x, y, 0)

  }

  def xy(x: Float, y: Float): Vec2 = {
    if (abs(x) < EPSILON && abs(y) < EPSILON) origin2 else new XY(x, y)
  }

  def xy(x: Number, y: Number): Vec2 = xy(x.floatValue, y.floatValue)

  object AwtImplicits {
    implicit def pointToVec2(p: java.awt.Point): Vec2 = xy(p.x, p.y)
    implicit def mouseEventToVec2(e: java.awt.event.MouseEvent): Vec2 = xy(e.getX, e.getY)
  }

  private class Ang2 (val _ang: Float, val _mag: Float) extends Vec2 {

    var _xy:Option[XY] = None

    def this(ang: Float, mag: Float) {
      this(mod2pi(ang, mag < 0), abs(mag))
    }

    def this(ang: Float) {
      this(mod2pi(ang), 1f)
    }

    private def xy = {
      if (_xy.isEmpty) {
        _xy = Some(new XY(
          mag * cos(ang).toFloat,
          mag * sin(ang).toFloat
        ))
      }
      _xy.get
    }

    override def x = xy.x
    override def y = xy.y

    override def ang = _ang
    override def mag = _mag

    override def rot180 = angleVec2(ang + PI, mag)
    override def rot90 = angleVec2(ang + HALFPI, mag)
    override def rot270 = angleVec2(ang - HALFPI, mag)

    override def mult(f: Float) =
      if (abs(f) < EPSILON) origin2 else angleVec2(ang, f * mag)

    override def div(d: Float) = angleVec2(ang, mag / d)

    override def in3d = azimuthAndElevation(ang, 0, mag)

  }

  def angleVec2(ang: Float, mag: Float): Vec2 =
    if (abs(mag) < EPSILON) origin2 else new Ang2(ang, mag)

  def angleVec2(ang: Number, mag: Number): Vec2 =
    angleVec2(ang.floatValue, mag.floatValue)

  def angleVec2(ang: Float, mag: Number): Vec2 =
    angleVec2(ang, mag.floatValue)

  def angleVec2(ang: Number, mag: Float): Vec2 =
    angleVec2(ang.floatValue, mag)

  def angleVec2(ang: Float): Vec2 = new Ang2(ang)

  def angleVec2(ang: Number): Vec2 = new Ang2(ang.floatValue)

  object origin2 extends Vec2 {
    override def x = 0
    override def y = 0
    override def ang = 0
    override def mag = 0
    override def mult(factor: Float) = this
    override def div(divisor: Float) = this
    override def rot90 = this
    override def rot270 = this
    override def rot180 = this
    override def add(o: Vec2) = o
    override def sub(o: Vec2) = o.mult(-1)
    override def addX($: Float) = xy($, 0)
    override def addY($: Float) = xy(0, $)
    override def subX($: Float) = xy(-$, 0)
    override def subY($: Float) = xy(0, -$)
    override def mag(newMag: Float) = this
    override def unit = this
    override def mult(factor: Number) = this
    override def div(divisor: Number) = this
    override def dot(o: Vec2) = 0
    override def cross(o: Vec2) = 0
    override def rot(ang: Float) = this
    override def rot(ang: Number) = this
    override def compareTo(o: Vec2) = java.lang.Float.compare(0, o.mag)
    override def isOrigin: Boolean = true
    override def in3d = origin3
    override def toString: String = "(0, 0)"
  }

  /**
   * A directed line segment in a Euclidean plane.
   */
  trait Line2 {
    def a: Vec2

    def b: Vec2

    def ab: Vec2

    def mag: Float

    def ang: Float

    def side(p: Vec2): Side = if (p.sub(a).cross(b.sub(a)) > 0) Left else Right

    def add(offset: Vec2): Line2

    def sub(offset: Vec2): Line2

    def midpoint: Vec2

    def bisect: Line2 = pointAndStep(midpoint, angleVec2(ang).rot90)

    /**
     * Not equal, but correlated, to "bulge" as defined by Jarek Rossignac.
     */
    def bulge(p: Vec2): Float = {
      val c: Circle2 = circle(a, b, p)
      c.radius * side(p).i * side(c.center).i
    }

  }

  sealed trait Side
  object Left extends Side
  object Right extends Side

  private class OriginLine2 (val _b: Vec2) extends Line2 {
    override def a = origin2
    override def b = _b
    override def ab = b
    override def ang = b.ang
    override def mag = b.mag
    override def add(offset: Vec2) = aToB(offset, b.add(offset))
    override def sub(offset: Vec2) = aToB(offset.mult(-1), b.sub(offset))
    override def midpoint = b.div(2)
  }

  def oTo2(ang: Float): Line2 = new OriginLine2(angleVec2(ang))
  def oTo2(ang: Number): Line2 = new OriginLine2(angleVec2(ang))
  def oTo2(p: Vec2): Line2 = new OriginLine2(p)

  class AtoB2 (val _a: Vec2, val _b: Vec2) extends Line2 {

    var _ab: Option[Vec2] = None

    def this(a: Vec2, b: Vec2, ab: Option[Vec2]) {
      this(a, b)
      _ab = ab
    }

    override def a = _a
    override def b = _b
    override def ab = {
      if (_ab.isEmpty) {
        _ab = Some(b.sub(a))
      }
      _ab.get
    }

    override def ang = ab.ang
    override def mag = ab.mag

    override def add(offset: Vec2) =
      new AtoB2(offset.add(a), b.add(offset), _ab)

    override def sub(offset: Vec2) =
      new AtoB2(offset.sub(a), b.sub(offset), _ab)

    override def midpoint: Vec2 = a.add(b).div(2)

    override def toString = "Line %s to %s".format(a, b)

  }

  def aToB(a: Vec2, b: Vec2): Line2 = new AtoB2(a, b)

  class PointAndDirection2 (val _a: Vec2, val _ab: Vec2) extends Line2 {
    override def a = _a
    override def b = a.add(ab)
    override def ab = _ab
    override def ang = ab.ang
    override def mag = ab.mag
    override def add(offset: Vec2) = pointAndStep(a.add(offset), ab)
    override def sub(offset: Vec2) = pointAndStep(a.sub(offset), ab)
    override def midpoint = ab.div(2).add(a)
  }

  def pointAndStep(a: Vec2, ab: Vec2) = new PointAndDirection2(a, ab)
  def pointAndStep(a: Vec2, ang: Float) = new PointAndDirection2(a, angleVec2(ang))
  def pointAndStep(a: Vec2, ang: Number) = new PointAndDirection2(a, angleVec2(ang))

  def intersect(ab: Line2, cd: Line2): Vec2 = {

    val v1 = ab.a; val v2 = ab.b; val v3 = cd.a; val v4 = cd.b
    val x1 = v1.x; val y1 = v1.y; val x2 = v2.x; val y2 = v2.y
    val x3 = v3.x; val y3 = v3.y; val x4 = v4.x; val y4 = v4.y

    // http://en.wikipedia.org/wiki/Line-line_intersection
    val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / d
    val y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / d

    xy(x, y)
  }

  def overlap(ab: Line2, cd: Line2): Boolean = {
    val a = ab.a; val b = ab.b; val c = cd.a; val d = cd.b
    (ab.side(c) ne ab.side(d)) & (cd.side(a) ne cd.side(b))
  }

  trait Circle2 {
    def center: Vec2
    def radius: Float
  }

  class SimpleCircle2 (val _center: Vec2, val _radius: Float) extends Circle2 {
    override def center = _center
    override def radius = _radius
  }

  def circle(center: Vec2, radius: Float): Circle2 =
    new SimpleCircle2(center, radius)

  def circle(center: Vec2, radius: Number): Circle2 =
    new SimpleCircle2(center, radius.floatValue)

  private class TriangleCircle2 (val vs: Array[Vec2]) extends Circle2 {

    var _center: Option[Vec2] = None
    var _radius: Option[Float] = None

    override def center: Vec2 = {
      if (_center.isEmpty) {
        _center = Some(intersect(aToB(vs(0), vs(1)).bisect, aToB(vs(1), vs(2)).bisect))
      }
      _center.get
    }

    override def radius: Float = {
      if (_radius.isEmpty) {
        _radius = Some(center.sub(vs(0)).mag)
      }
      _radius.get
    }

  }

  def circle(a: Vec2, b: Vec2, c: Vec2): Circle2 =
    new TriangleCircle2(Array[Vec2](a, b, c))

  /**
   * 0, 1, or 2 intersections.
   */
  def intersect(line$: Line2, circle: Circle2): Array[Vec2] = {
    // http://mathworld.wolfram.com/Circle-LineIntersection.html
    var line = line$
    val r = circle.radius
    val cc = circle.center
    line = line.sub(cc)
    val a = line.a; val b = line.b; val ab = line.ab
    val dx = ab.x; val dy = ab.y
    val dr = sqrt(pow(dx, 2) + pow(dy, 2)).toFloat
    val D = a.x * b.y - b.x * a.y
    val q = sqrt(pow(r, 2) * pow(dr, 2) - pow(D, 2)).toFloat
    if (q < 0) return new Array[Vec2](0)
    val qx = sign(dy) * dx * q; val qy = abs(dy) * q
    val Ddy = D * dy; val nDdx = 0 - D * dx
    if (qx == 0 && qy == 0) return Array[Vec2](xy(Ddy, nDdx))
    val is = Array[Vec2](xy(Ddy + qx, nDdx + qy), xy(Ddy - qx, nDdx - qy))
    for (i <- 0 until 3) { is(i) = is(i).div(pow(dr, 2)).add(cc) }
    is
  }

  /**
   * A point in three dimensions.
   */
  trait Vec3 extends IsVec3 with Comparable[Vec3] {
    def x: Float
    def y: Float
    def z: Float
    def mag: Float
    def magSquared: Float
    def mag(newMag: Float): Vec3 = unit.mult(newMag)

    /**
     * Equivalent to mag(1).
     */
    def unit: Vec3 = div(mag)

    def add(o: Vec3): Vec3 = xyz(x + o.x, y + o.y, z + o.z)
    def sub(o: Vec3): Vec3 = xyz(x - o.x, y - o.y, z - o.z)
    def mult(factor: Float): Vec3
    def mult(factor: Number): Vec3 = mult(factor.floatValue)
    def div(divisor: Float): Vec3
    def div(divisor: Number): Vec3 = div(divisor.floatValue)
    def addX($: Float): Vec3 = xyz(x + $, y, z)
    def addY($: Float): Vec3 = xyz(x, y + $, z)
    def addZ($: Float): Vec3 = xyz(x, y, z + $)
    def subX($: Float): Vec3 = xyz(x - $, y, z)
    def subY($: Float): Vec3 = xyz(x, y - $, z)
    def subZ($: Float): Vec3 = xyz(x, y, z - $)

    /**
     * This is exactly (0, 0, 0)?
     */
    def isOrigin: Boolean = false

    /**
     * Scalar (dot) product.
     */
    def dot(o: Vec3): Float = x * o.x + y * o.y + z * o.z

    /**
     * Cross product U X V, normal to both U and V.
     */
    def cross(o: Vec3): Vec3 =
      xyz(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)

    /** Rotate 90 degrees in the XY plane. */
    def rot90xy: Vec3

    def azimuth: Float
    def elevation: Float

    def azimuth(newAzimuth: Float): Vec3 =
      azimuthAndElevation(newAzimuth, elevation, mag)

    def elevation(newElevation: Float): Vec3 =
      azimuthAndElevation(azimuth, newElevation, mag)

    def xy: Vec2

    /** An arbitrary orthogonal vector. */
    def orthog: Vec3 = azimuthAndElevation(azimuth, elevation + HALFPI, mag)

    def compareTo(o: Vec3) = java.lang.Float.compare(mag, o.mag)

    override def toString: String = "(%f, %f, %f)".format(x, y, z)

  }

  private abstract class BaseVec3 extends Vec3 {


    def asVec3: Vec3 = {
      this
    }
  }

  private class XYZ extends BaseVec3 {
    private[vine] def this(x: Float, y: Float, z: Float) {
      this()
      this.x = x
      this.y = y
      this.z = z
    }

    def x: Float = {
      x
    }

    def y: Float = {
      y
    }

    def z: Float = {
      z
    }

    def mag: Float = {
      if (!hasMag) {
        mag = sqrt(magSquared).asInstanceOf[Float]
        hasMag = true
      }
      mag
    }

    def magSquared: Float = {
      if (!hasMagSquared) {
        magSquared = (pow(x, 2) + pow(y, 2) + pow(z, 2)).asInstanceOf[Float]
        hasMagSquared = true
      }
      magSquared
    }

    def mult(f: Float): Vec3 = {
      if (abs(f) < EPSILON) origin3 else xyz(f * x, f * y, f * z)
    }

    def div(d: Float): Vec3 = {
      xyz(x / d, y / d, z / d)
    }

    def rot90xy: Vec3 = {
      xyz(-1 * y, x, z)
    }

    def elevation: Float = {
      if (!hasElevation) {
        elevation = elevation(asin(z / mag).asInstanceOf[Float])
        hasElevation = true
      }
      elevation
    }

    def azimuth: Float = {
      if (!hasAzimuth) {
        azimuth = mod2pi(atan2(y, x).asInstanceOf[Float])
        hasAzimuth = true
      }
      azimuth
    }

    def xy: Vec2 = {
      xy(x, y)
    }

    private[vine] final val x: Float = .0
    private[vine] final val y: Float = .0
    private[vine] final val z: Float = .0
    private[vine] var mag: Float = .0
    private[vine] var magSquared: Float = .0
    private[vine] var elevation: Float = .0
    private[vine] var azimuth: Float = .0
    private[vine] var hasMag: Boolean = false
    private[vine] var hasMagSquared: Boolean = false
    private[vine] var hasElevation: Boolean = false
    private[vine] var hasAzimuth: Boolean = false
  }

  def xyz(x: Float, y: Float, z: Float): Vec3 = {
    if (abs(x) < EPSILON && abs(y) < EPSILON && abs(z) < EPSILON) ORIGIN_3 else new XYZ(x, y, z)
  }

  def xyz(x: Number, y: Number, z: Number): Vec3 = {
    xyz(x.floatValue, y.floatValue, z.floatValue)
  }

  def xyz(array: Array[Float]): Vec3 = {
    xyz(array(0), array(1), array(2))
  }

  def xyz(array: Array[Double]): Vec3 = {
    xyz(array(0), array(1), array(2))
  }

  def xyz(array: Array[Number]): Vec3 = {
    xyz(array(0), array(1), array(2))
  }

  private class Origin3 extends Vec3 {
    def x: Float = {
      0
    }

    def y: Float = {
      0
    }

    def z: Float = {
      0
    }

    def mag: Float = {
      0
    }

    def magSquared: Float = {
      0
    }

    def mult(factor: Float): Vec3 = {
      this
    }

    def div(divisor: Float): Vec3 = {
      this
    }

    def add(o: Vec3): Vec3 = {
      o
    }

    def sub(o: Vec3): Vec3 = {
      o.mult(-1)
    }

    def addX($: Float): Vec3 = {
      xyz($, 0, 0)
    }

    def addY($: Float): Vec3 = {
      xyz(0, $, 0)
    }

    def addZ($: Float): Vec3 = {
      xyz(0, 0, $)
    }

    def subX($: Float): Vec3 = {
      xyz(-$, 0, 0)
    }

    def subY($: Float): Vec3 = {
      xyz(0, -$, 0)
    }

    def subZ($: Float): Vec3 = {
      xyz(0, 0, -$)
    }

    def mag(newMag: Float): Vec3 = {
      this
    }

    def unit: Vec3 = {
      this
    }

    def mult(factor: Number): Vec3 = {
      this
    }

    def div(divisor: Number): Vec3 = {
      this
    }

    def dot(o: Vec3): Float = {
      0
    }

    def cross(o: Vec3): Vec3 = {
      this
    }

    def rot90xy: Vec3 = {
      this
    }

    def compareTo(o: Vec3): Int = {
      Float.compare(0, o.mag)
    }

    def isOrigin: Boolean = {
      true
    }

    def azimuth: Float = {
      0
    }

    def elevation: Float = {
      0
    }

    def azimuth(newAzimuth: Float): Vec3 = {
      this
    }

    def elevation(newElevation: Float): Vec3 = {
      this
    }

    def xy: Vec2 = {
      origin2
    }

    def orthog: Vec3 = {
      this
    }

    override def toString: String = {
      "(0, 0, 0)"
    }

    def asVec3: Vec3 = {
      this
    }
  }

  private final val ORIGIN_3: Origin3 = new Origin3

  def distance(a: Vec2, b: Vec2): Float = {
    a.sub(b).mag
  }

  def distance(a: Vec3, b: Vec3): Float = {
    a.sub(b).mag
  }

  def distance(a: IsVec3, b: IsVec3): Float = {
    a.asVec3.sub(b.asVec3).mag
  }

  def midpoint(a: Vec2, b: Vec2): Vec2 = {
    a.add(b).div(2)
  }

  def midpoint(a: Vec3, b: Vec3): Vec3 = {
    a.add(b).div(2)
  }

  /**
   * A directed line segment in three dimensional space.
   */
  abstract trait Line3 {
    def a: Vec3

    def b: Vec3

    def ab: Vec3

    def mag: Float

    def add(offset: Vec3): Line3

    def sub(offset: Vec3): Line3

    def midpoint: Vec3

    /**
     * Changes A without affecting B.
     */
    def a(a: Vec3): Line3

    /**
     * Changes B without affecting A.
     */
    def b(b: Vec3): Line3

    /**
     * Changes A without affecting AB.
     */
    def aShift(a: Vec3): Line3

    /**
     * Changes B without affecting AB.
     */
    def bShift(b: Vec3): Line3

    /**
     * Changes AB without affecting A.
     */
    def ab(ab: Vec3): Line3

    /**
     * Elevation of AB.
     */
    def elevation: Float

    /**
     * Azimuth of AB.
     */
    def azimuth: Float

    /**
     * An arbitrary line, passing through A, orthogonal to this line.
     */
    def aOrthog: Line3

    /**
     * An arbitrary line, passing through B, orthogonal to this line.
     */
    def bOrthog: Line3

    def reverse: Line3

    /**
     * Move b such that the magnitude of ab is multiplied.
     */
    def mult(factor: Float): Line3
  }

  private abstract class BaseLine3 extends Line3 {
    def mag: Float = {
      ab.mag
    }

    def a(a: Vec3): Line3 = {
      aToB(a, b)
    }

    def b(b: Vec3): Line3 = {
      aToB(a, b)
    }

    def aShift(a: Vec3): Line3 = {
      pointAndStep(a, ab)
    }

    def bShift(b: Vec3): Line3 = {
      val ab: Vec3 = ab
      new AtoB3(b.sub(ab), b, ab)
    }

    def ab(ab: Vec3): Line3 = {
      pointAndStep(a, ab)
    }

    def elevation: Float = {
      ab.elevation
    }

    def azimuth: Float = {
      ab.azimuth
    }

    def aOrthog: Line3 = {
      pointAndStep(a, ab.orthog)
    }

    def bOrthog: Line3 = {
      reverse.aOrthog
    }

    def mult(factor: Float): Line3 = {
      pointAndStep(a, ab.mult(factor))
    }

    override def toString: String = {
      String.format("%s -> %s", a, b)
    }
  }

  private class AtoB3 extends BaseLine3 {
    private[vine] def this(a: Vec3, b: Vec3) {
      this()
      this.a = a
      this.b = b
    }

    private[vine] def this(a: Vec3, b: Vec3, ab: Vec3) {
      this()
      this.a = a
      this.b = b
      this.ab = ab
    }

    def a: Vec3 = {
      a
    }

    def b: Vec3 = {
      b
    }

    def ab: Vec3 = {
      if (ab == null) ab = b.sub(a)
      ab
    }

    def add(offset: Vec3): Line3 = {
      new AtoB3(offset.add(a), b.add(offset), ab)
    }

    def sub(offset: Vec3): Line3 = {
      new AtoB3(a.sub(offset), b.sub(offset), ab)
    }

    def reverse: Line3 = {
      new AtoB3(b, a, if (ab == null) null else ab.mult(-1))
    }

    def midpoint: Vec3 = {
      a.add(b).div(2)
    }

    override def toString: String = {
      String.format("Line %s to %s", a, b)
    }

    private[vine] final val a: Vec3 = null
    private[vine] final val b: Vec3 = null
    private[vine] var ab: Vec3 = null
  }

  def aToB(a: IsVec3, b: IsVec3): Line3 = {
    aToB(a.asVec3, b.asVec3)
  }

  def aToB(a: Vec3, b: Vec3): Line3 = {
    new AtoB3(a, b)
  }

  private class AzimuthAndElevation extends BaseVec3 {
    private def this(azimuth: Float, elevation: Float, mag: Float) {
      this()
      this.azimuth = mod2pi(azimuth, mag < 0)
      this.elevation = elevation(elevation) * sign(mag)
      this.mag = abs(mag)
    }

    def azimuth: Float = {
      azimuth
    }

    def elevation: Float = {
      elevation
    }

    private[vine] def xyz: Vec3 = {
      if (xyz == null) {
        val cosAzimuth: Double = cos(azimuth)
        val cosElevation: Double = cos(elevation)
        val sinAzimuth: Double = sin(azimuth)
        val sinElevation: Double = sin(elevation)
        xyz = xyz(cosElevation * cosAzimuth, cosElevation * sinAzimuth, sinElevation).mag(mag)
      }
      xyz
    }

    def x: Float = {
      xyz.x
    }

    def y: Float = {
      xyz.y
    }

    def z: Float = {
      xyz.z
    }

    def mag: Float = {
      mag
    }

    def magSquared: Float = {
      mag * mag
    }

    override def mag(newMag: Float): Vec3 = {
      azimuthAndElevation(azimuth, elevation, newMag)
    }

    def mult(factor: Float): Vec3 = {
      azimuthAndElevation(azimuth, elevation, mag * factor)
    }

    def div(divisor: Float): Vec3 = {
      azimuthAndElevation(azimuth, elevation, mag / divisor)
    }

    def rot90xy: Vec3 = {
      xyz.rot90xy
    }

    def xy: Vec2 = {
      angleVec2(azimuth, mag)
    }

    private[vine] final val azimuth: Float = .0
    private[vine] final val elevation: Float = .0
    private[vine] final val mag: Float = .0
    private[vine] var xyz: Vec3 = null
  }

  /**
   * Start with (1, 0, 0).
   * Rotate by the azimuth angle on the XY plane.
   * Rotate toward the Z axis by the elevation angle.
   */
  def azimuthAndElevation(azimuth: Float, elevation: Float, mag: Float): Vec3 = {
    if (abs(mag) < EPSILON) origin3 else new AzimuthAndElevation(azimuth, elevation, mag)
  }

  private class PointAndDirection3 extends BaseLine3 {
    private def this(a: Vec3, ab: Vec3) {
      this()
      this.a = a
      this.ab = ab
    }

    def a: Vec3 = {
      a
    }

    def b: Vec3 = {
      a.add(ab)
    }

    def ab: Vec3 = {
      ab
    }

    def add(offset: Vec3): Line3 = {
      new PointAndDirection3(a.add(offset), ab)
    }

    def sub(offset: Vec3): Line3 = {
      new PointAndDirection3(a.sub(offset), ab)
    }

    def reverse: Line3 = {
      new PointAndDirection3(b, ab.mult(-1))
    }

    def midpoint: Vec3 = {
      a.add(ab.div(2))
    }

    private[vine] final val a: Vec3 = null
    private[vine] final val ab: Vec3 = null
  }

  def pointAndStep(a: Vec3, ab: Vec3): Line3 = {
    new PointAndDirection3(a, ab)
  }

  def pointAndStep(a: IsVec3, ab: IsVec3): Line3 = {
    new PointAndDirection3(a.asVec3, ab.asVec3)
  }

  def oTo3(b: Vec3): Line3 = {
    aToB(origin3, b)
  }

  def distance(line: Line3, c: IsVec3): Float = {
    distance(line, c.asVec3)
  }

  def distance(line: Line3, c: Vec3): Float = {
    val a: Vec3 = line.a
    val b: Vec3 = line.b
    val ac: Vec3 = aToB(a, c).ab
    val bc: Vec3 = aToB(b, c).ab
    val ab: Vec3 = line.ab
    ac.cross(bc).mag / ab.mag
  }

  private def matrixApply(m: Array[Array[Float]], x: Vec3): Vec3 = {
    xyz(m(0)(0) * x.x + m(0)(1) * x.y + m(0)(2) * x.z, m(1)(0) * x.x + m(1)(1) * x.y + m(1)(2) * x.z, m(2)(0) * x.x + m(2)(1) * x.y + m(2)(2) * x.z)
  }

  private def rotationMatrix(axis: Vec3, angle: Float): Array[Array[Float]] = {
    val u: Vec3 = axis.unit
    val ux: Float = u.x
    val uy: Float = u.y
    val uz: Float = u.z
    val cosa: Float = cos(angle).asInstanceOf[Float]
    val sina: Float = sin(angle).asInstanceOf[Float]
    Array[Array[Float]](Array(cosa + ux * ux * (1 - cosa), ux * uy * (1 - cosa) - uz * sina, ux * uz * (1 - cosa) + uy * sina), Array(uy * ux * (1 - cosa) + uz * sina, cosa + uy * uy * (1 - cosa), uy * uz * (1 - cosa) - ux * sina), Array(uz * ux * (1 - cosa) - uy * sina, uz * uy * (1 - cosa) + ux * sina, cosa + uz * uz * (1 - cosa)))
  }

  def rotatePointAroundLine(line: Line3, c: IsVec3, angle: Float): Vec3 = {
    rotatePointAroundLine(line, c.asVec3, angle)
  }

  def rotatePointAroundLine(line: Line3, c: Vec3, angle: Float): Vec3 = {
    val matrix: Array[Array[Float]] = rotationMatrix(line.ab, angle)
    c = c.sub(line.a)
    c = matrixApply(matrix, c)
    c = c.add(line.a)
    c
  }

  abstract trait Sphere {
    def center: Vec3

    def radius: Float
  }

  private class SimpleSphere extends Sphere {
    private def this(center: Vec3, radius: Float) {
      this()
      this.center = center
      this.radius = radius
    }

    def center: Vec3 = {
      center
    }

    def radius: Float = {
      radius
    }

    private[vine] final val center: Vec3 = null
    private[vine] final val radius: Float = .0
  }

  def sphere(center: Vec3, radius: Float): Sphere = {
    new SimpleSphere(center, radius)
  }

  def sphere(center: IsVec3, radius: Float): Sphere = {
    sphere(center.asVec3, radius)
  }

  abstract trait Circle3 {
    def center: Vec3

    /** A vector orthogonal to the plane on which the circle lies. */
    def normal: Vec3

    def radius: Float

    /** This sphere's intersection with the XY plane. 0 or 1 circles. */
    def intersectXY: List[Circle2]

    /** This sphere's intersection with the origin-intersectingplane normal to the vector. 0 or 1 circles. */
    def intersectPlane(normal: Vec3): List[Circle3]
  }

  class SimpleCircle3 extends Circle3 {
    def this(center: Vec3, normal: Vec3, radius: Float) {
      this()
      this.center = center
      this.normal = normal
      this.radius = radius
    }

    def center: Vec3 = {
      center
    }

    def normal: Vec3 = {
      normal
    }

    def radius: Float = {
      radius
    }

    def intersectXY: List[Circle2] = {
      val r: Float = radius
      val d: Float = center.z
      if (d > r) {
        return asList(Array[Circle2])
      }
      asList(circle(center.xy, r * r - d * d))
    }

    def intersectPlane(normal: Vec3): List[Circle3] = {
      null
    }

    private[vine] final val center: Vec3 = null
    private[vine] final val normal: Vec3 = null
    private[vine] final val radius: Float = .0
  }

  def circle(center: Vec3, normal: Vec3, radius: Float): Circle3 = {
    new SimpleCircle3(center, normal, radius)
  }

  /** 0 or 1 circles. */
  def intersect(a: Sphere, b: Sphere): List[Circle3] = {
    val R: Float = a.radius
    val r: Float = b.radius
    if (distance(a.center, b.center) > R + r) {
      return asList(Array[Circle3])
    }
    val ab: Line3 = aToB(a.center, b.center)
    val d: Float = ab.mag
    val x: Float = (d * d - r * r + R * R) / (2 * d)
    val center: Vec3 = ab.a.add(ab.ab.mag(x))
    val rad: Float = sqrt((r - d - R) * (R - d - r) * (r - d + R) * (d + r + R)).asInstanceOf[Float] / (2 * d)
    asList(circle(center, ab.ab, rad))
  }

  def parseXYZ(s: String): Vec3 = {
    val ss: Array[String] = s.split(",")
    xyz(Float.parseFloat(ss(0)), Float.parseFloat(ss(1)), Float.parseFloat(ss(2)))
  }

  def formatXYZ(v: Vec3): String = {
    String.format("%f,%f,%f", v.x, v.y, v.z)
  }
}
