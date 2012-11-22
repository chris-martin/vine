package vine

import math._

object geometry {

  private val EPSILON: Float = 0.000001f
  private val PI: Float = Pi.toFloat
  private val PI2: Float = PI * 2
  private val HALFPI: Float = PI / 2
  private val NEG_HALFPI: Float = PI / -2
  private val THREE_HALVES_PI: Float = 1.5f * PI

  private def mod2pi(a: Float): Float =
    if (a < 0) ((a % PI2) + PI2) else (a % PI2)

  private def mod2pi(a: Float, flip: Boolean): Float =
    mod2pi(if (flip) a + PI else a)

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

  private def sign(x: Float): Int =
    if (x == 0) 0 else (if (x < 0) -1 else 1)

  /**
   * A point in a Euclidean plane.
   */
  trait Vec2 extends Comparable[Vec2] {
    def x: Float
    def y: Float
    def ang: Float
    def mag: Float
    def mag(newMag: Float): Vec2 = angleVec2(ang, newMag)

    /** Equivalent to mag(1). */
    def unit: Vec2 = angleVec2(ang, 1)

    def +(o: Vec2): Vec2 = xy(x + o.x, y + o.y)
    def -(o: Vec2): Vec2 = xy(x - o.x, y - o.y)
    def *(factor: Float): Vec2
    def *(factor: Number): Vec2 = *(factor.floatValue)
    def /(divisor: Float): Vec2
    def /(divisor: Number): Vec2 = /(divisor.floatValue)
    def rot(ang: Float): Vec2 = angleVec2(ang + ang, mag)
    def rot(ang: Number): Vec2 = rot(ang.floatValue)
    def addX($: Float): Vec2 = xy(x + $, y)
    def addY($: Float): Vec2 = xy(x, y + $)
    def subX($: Float): Vec2 = xy(x - $, y)
    def subY($: Float): Vec2 = xy(x, y - $)

    /** This is exactly (0, 0)? */
    def isOrigin: Boolean = false

    def rot90: Vec2
    def rot180: Vec2
    def rot270: Vec2

    /** Scalar (dot) product. */
    def dot(o: Vec2): Float = x * o.x + y * o.y

    /** U x V = U dot rot90(V). */
    def cross(o: Vec2): Float = dot(o.rot90)

    /** Raises this 2d vector into three dimensions by adding a zero-value z value. */
    def in3d: Vec3

    def compareTo(o: Vec2): Int = java.lang.Float.compare(mag, o.mag)

    override def toString: String = "(%f, %f)".format(x, y)

  }

  private class XY (val _x: Float, val _y: Float) extends Vec2 {

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

    override def *(f: Float) =
      if (abs(f) < EPSILON) origin2 else xy(f * x, f * y)

    override def /(d: Float): Vec2 = xy(x / d, y / d)

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

    override def *(f: Float) =
      if (abs(f) < EPSILON) origin2 else angleVec2(ang, f * mag)

    override def /(d: Float) = angleVec2(ang, mag / d)

    override def in3d = azimuthAndElevation(ang, 0, mag)

  }

  def angleVec2(ang: Float, mag: Float): Vec2 =
    if (abs(mag) < EPSILON) origin2 else new Ang2(mod2pi(ang, mag < 0), abs(mag))

  def angleVec2(ang: Number, mag: Number): Vec2 =
    angleVec2(ang.floatValue, mag.floatValue)

  def angleVec2(ang: Float, mag: Number): Vec2 =
    angleVec2(ang, mag.floatValue)

  def angleVec2(ang: Number, mag: Float): Vec2 =
    angleVec2(ang.floatValue, mag)

  def angleVec2(ang: Float): Vec2 = angleVec2(ang, 1f)

  def angleVec2(ang: Number): Vec2 = angleVec2(ang.floatValue)

  object origin2 extends Vec2 {
    override def x = 0
    override def y = 0
    override def ang = 0
    override def mag = 0
    override def *(factor: Float) = this
    override def /(divisor: Float) = this
    override def rot90 = this
    override def rot270 = this
    override def rot180 = this
    override def +(o: Vec2) = o
    override def -(o: Vec2) = o.*(-1)
    override def addX($: Float) = xy($, 0)
    override def addY($: Float) = xy(0, $)
    override def subX($: Float) = xy(-$, 0)
    override def subY($: Float) = xy(0, -$)
    override def mag(newMag: Float) = this
    override def unit = this
    override def *(factor: Number) = this
    override def /(divisor: Number) = this
    override def dot(o: Vec2) = 0
    override def cross(o: Vec2) = 0
    override def rot(ang: Float) = this
    override def rot(ang: Number) = this
    override def compareTo(o: Vec2) = java.lang.Float.compare(0, o.mag)
    override def isOrigin: Boolean = true
    override def in3d:Vec3 = origin3
    override def toString: String = "(0, 0)"
  }

  /** A directed line segment in a Euclidean plane. */
  trait Line2 {
    def a: Vec2
    def b: Vec2
    def ab: Vec2
    def mag: Float
    def ang: Float
    def side(p: Vec2): Side = if (p.-(a).cross(b.-(a)) > 0) Left else Right
    def add(offset: Vec2): Line2
    def sub(offset: Vec2): Line2
    def midpoint: Vec2
    def bisect: Line2 = pointAndStep(midpoint, angleVec2(ang).rot90)

    /** Not equal, but correlated, to "bulge" as defined by Jarek Rossignac. */
    def bulge(p: Vec2): Float = {
      val c: Circle2 = circle(a, b, p)
      c.radius * side(p).i * side(c.center).i
    }

  }

  sealed trait Side { def i:Int }
  object Left extends Side { override def i = -1 }
  object Right extends Side { override def i = 1 }

  private class OriginLine2 (val _b: Vec2) extends Line2 {
    override def a = origin2
    override def b = _b
    override def ab = b
    override def ang = b.ang
    override def mag = b.mag
    override def add(offset: Vec2) = aToB(offset, b.+(offset))
    override def sub(offset: Vec2) = aToB(offset.*(-1), b.-(offset))
    override def midpoint = b./(2)
  }

  def oTo2(ang: Float): Line2 = new OriginLine2(angleVec2(ang))
  def oTo2(ang: Number): Line2 = new OriginLine2(angleVec2(ang))
  def oTo2(p: Vec2): Line2 = new OriginLine2(p)

  class AtoB2 (val _a: Vec2, val _b: Vec2) extends Line2 {
    var _ab: Option[Vec2] = None
    def this(a: Vec2, b: Vec2, ab: Option[Vec2]) { this(a, b); _ab = ab }
    override def a = _a
    override def b = _b
    override def ab = { if (_ab.isEmpty) { _ab = Some(b.-(a)) }; _ab.get }
    override def ang = ab.ang
    override def mag = ab.mag
    override def add(offset: Vec2) = new AtoB2(offset.+(a), b.+(offset), _ab)
    override def sub(offset: Vec2) = new AtoB2(offset.-(a), b.-(offset), _ab)
    override def midpoint: Vec2 = a.+(b)./(2)
    override def toString = "Line %s to %s".format(a, b)
  }

  def aToB(a: Vec2, b: Vec2): Line2 = new AtoB2(a, b)

  class PointAndDirection2 (val _a: Vec2, val _ab: Vec2) extends Line2 {
    override def a = _a
    override def b = a.+(ab)
    override def ab = _ab
    override def ang = ab.ang
    override def mag = ab.mag
    override def add(offset: Vec2) = pointAndStep(a.+(offset), ab)
    override def sub(offset: Vec2) = pointAndStep(a.-(offset), ab)
    override def midpoint = ab./(2).+(a)
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
        _radius = Some(center.-(vs(0)).mag)
      }
      _radius.get
    }

  }

  def circle(a: Vec2, b: Vec2, c: Vec2): Circle2 =
    new TriangleCircle2(Array[Vec2](a, b, c))

  /**
   * 0, 1, or 2 intersections.
   */
  def intersect(line$: Line2, circle: Circle2): List[Vec2] = {
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
    if (q < 0) return List()
    val qx = sign(dy) * dx * q; val qy = abs(dy) * q
    val Ddy = D * dy; val nDdx = 0 - D * dx
    if (qx == 0 && qy == 0) return List(xy(Ddy, nDdx))
    val is = List[Vec2](xy(Ddy + qx, nDdx + qy), xy(Ddy - qx, nDdx - qy))
    is.map(i => (i / (pow(dr, 2))) + cc)
  }

  /**
   * A point in three dimensions.
   */
  trait Vec3 extends Comparable[Vec3] {
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

  private class XYZ (val _x: Float, val _y: Float, val _z: Float) extends Vec3 {

    var _mag: Option[Float] = None
    var _magSquared: Option[Float] = None
    var _elevation: Option[Float] = None
    var _azimuth: Option[Float] = None

    override def x = _x
    override def y = _y
    override def z = _z
    override def mag = {
      if (_mag.isEmpty) { _mag = Some(sqrt(magSquared).toFloat) }
      _mag.get
    }

    override def magSquared = {
      if (_magSquared.isEmpty) {
        _magSquared = Some((pow(x, 2) + pow(y, 2) + pow(z, 2)).toFloat)
      }
      _magSquared.get
    }

    override def mult(f: Float) =
      if (abs(f) < EPSILON) origin3 else xyz(f*x, f*y, f*z)

    override def div(d: Float) = xyz(x/d, y/d, z/d)

    override def rot90xy = xyz(-1*y, x, z)

    override def elevation = {
      if (_elevation.isEmpty) {
        _elevation = Some(geometry.elevation(asin(z / mag).toFloat))
      }
      _elevation.get
    }

    override def azimuth: Float = {
      if (_azimuth.isEmpty) {
        _azimuth = Some(mod2pi(atan2(y, x).toFloat))
      }
      _azimuth.get
    }

    override def xy = geometry.xy(x, y)

  }

  def xyz(x: Float, y: Float, z: Float): Vec3 =
    if (abs(x) < EPSILON && abs(y) < EPSILON && abs(z) < EPSILON) origin3
    else new XYZ(x, y, z)

  def xyz(x: Number, y: Number, z: Number): Vec3 =
    xyz(x.floatValue, y.floatValue, z.floatValue)

  implicit def xyz(seq: Seq[Float]): Vec3 = geometry.xyz(seq(0), seq(1), seq(2))
  implicit def xyz(array: Array[Float]): Vec3 = xyz(array toSeq)

  object origin3 extends Vec3 {
    override def x = 0
    override def y = 0
    override def z = 0
    override def mag = 0
    override def magSquared = 0
    override def mult(factor: Float) = this
    override def div(divisor: Float) = this
    override def add(o: Vec3) = o
    override def sub(o: Vec3) = o.mult(-1)
    override def addX($: Float) = xyz($, 0, 0)
    override def addY($: Float) = xyz(0, $, 0)
    override def addZ($: Float) = xyz(0, 0, $)
    override def subX($: Float) = xyz(-$, 0, 0)
    override def subY($: Float) = xyz(0, -$, 0)
    override def subZ($: Float) = xyz(0, 0, -$)
    override def mag(newMag: Float) = this
    override def unit = this
    override def mult(factor: Number) = this
    override def div(divisor: Number) = this
    override def dot(o: Vec3) = 0
    override def cross(o: Vec3) = this
    override def rot90xy = this
    override def compareTo(o: Vec3) = java.lang.Float.compare(0, o.mag)
    override def isOrigin = true
    override def azimuth = 0
    override def elevation = 0
    override def azimuth(newAzimuth: Float) = this
    override def elevation(newElevation: Float) = this
    override def xy = origin2
    override def orthog = this
    override def toString = "(0, 0, 0)"
  }

  def distance(a: Vec2, b: Vec2): Float = a.-(b).mag
  def distance(a: Vec3, b: Vec3): Float = a.sub(b).mag
  def midpoint(a: Vec2, b: Vec2): Vec2 = a.+(b)./(2)
  def midpoint(a: Vec3, b: Vec3): Vec3 = a.add(b).div(2)

  /** A directed line segment in three dimensional space. */
  trait Line3 {
    def a: Vec3
    def b: Vec3
    def ab: Vec3
    def mag: Float = ab.mag
    def +(offset: Vec3): Line3
    def -(offset: Vec3): Line3
    def midpoint: Vec3

    /** Changes A without affecting B. */
    def a(a: Vec3): Line3 = aToB(a, b)

    /** Changes B without affecting A. */
    def b(b: Vec3): Line3 = aToB(a, b)

    /** Changes A without affecting AB. */
    def aShift(a: Vec3): Line3 = pointAndStep(a, ab)

    /** Changes B without affecting AB. */
    def bShift(b: Vec3): Line3 = new AtoB3(b.sub(ab), b, Some(ab))

    /** Changes AB without affecting A. */
    def ab(ab: Vec3): Line3 = pointAndStep(a, ab)

    /** Elevation of AB. */
    def elevation: Float = ab.elevation

    /** Azimuth of AB. */
    def azimuth: Float = ab.azimuth

    /** An arbitrary line, passing through A, orthogonal to this line. */
    def aOrthog: Line3 = pointAndStep(a, ab.orthog)

    /** An arbitrary line, passing through B, orthogonal to this line. */
    def bOrthog: Line3 = reverse.aOrthog

    def reverse: Line3

    /** Move b such that the magnitude of ab is multiplied. */
    def mult(factor: Float): Line3 = pointAndStep(a, ab.mult(factor))

    override def toString = "%s -> %s".format(a, b)

  }

  private class AtoB3 (val _a: Vec3, val _b: Vec3) extends Line3 {

    def this(a: Vec3, b: Vec3, ab: Option[Vec3]) {
      this(a, b)
      _ab = ab
    }

    var _ab: Option[Vec3] = None

    override def ab = {
      if (_ab.isEmpty) { _ab = Some(b.sub(a)) }
      _ab.get
    }

    override def a = _a
    override def b = _b
    override def +(offset: Vec3) = new AtoB3(offset.add(a), b.add(offset), _ab)
    override def -(offset: Vec3) = new AtoB3(a.sub(offset), b.sub(offset), _ab)
    override def reverse = new AtoB3(b, a, _ab.map($ => $.mult(-1)))
    override def midpoint = a.add(b).div(2)
    override def toString = "Line %s to %s".format(a, b)

  }

  def aToB(a: Vec3, b: Vec3): Line3 = new AtoB3(a, b)

  private class AzimuthAndElevation (
        val _azimuth: Float, val _elevation: Float, val _mag: Float
      ) extends Vec3 {

    override def azimuth = _azimuth
    override def elevation = _elevation
    override def mag = _mag

    var _xyz: Option[Vec3] = None

    private def xyz: Vec3 = {
      if (_xyz.isEmpty) {
        val cosAzimuth: Double = cos(azimuth)
        val cosElevation: Double = cos(elevation)
        val sinAzimuth: Double = sin(azimuth)
        val sinElevation: Double = sin(elevation)
        _xyz = Some(geometry.xyz(
          cosElevation * cosAzimuth,
          cosElevation * sinAzimuth,
          sinElevation
        ).mag(mag))
      }
      _xyz.get
    }

    override def x = xyz.x
    override def y = xyz.y
    override def z = xyz.z
    override def magSquared = mag * mag
    override def mag(newMag: Float) =
      azimuthAndElevation(azimuth, elevation, newMag)
    override def mult(factor: Float) =
      azimuthAndElevation(azimuth, elevation, mag * factor)
    override def div(divisor: Float) =
      azimuthAndElevation(azimuth, elevation, mag / divisor)
    override def rot90xy = xyz.rot90xy
    override def xy = angleVec2(azimuth, mag)

  }

  /**
   * Start with (1, 0, 0).
   * Rotate by the azimuth angle on the XY plane.
   * Rotate toward the Z axis by the elevation angle.
   */
  def azimuthAndElevation(azimuth: Float, elevation: Float, mag: Float): Vec3 =
    if (abs(mag) < EPSILON) origin3
    else new AzimuthAndElevation(
      mod2pi(azimuth, mag < 0),
      geometry.elevation(elevation) * sign(mag),
      abs(mag)
    )

  private class PointAndDirection3 (val _a: Vec3, val _ab: Vec3) extends Line3 {
    override def a: Vec3 = _a
    override def b: Vec3 = a.add(ab)
    override def ab: Vec3 = _ab
    override def +(offset: Vec3) = new PointAndDirection3(a.add(offset), ab)
    override def -(offset: Vec3) = new PointAndDirection3(a.sub(offset), ab)
    override def reverse = new PointAndDirection3(b, ab.mult(-1))
    override def midpoint = a.add(ab.div(2))
  }

  def pointAndStep(a: Vec3, ab: Vec3): Line3 =
    new PointAndDirection3(a, ab)

  def oTo3(b: Vec3): Line3 = aToB(origin3, b)

  def distance(line: Line3, c: Vec3): Float = {
    val a = line.a
    val b = line.b
    val ac = aToB(a, c).ab
    val bc = aToB(b, c).ab
    val ab = line.ab
    ac.cross(bc).mag / ab.mag
  }

  private def matrixApply(m: List[List[Float]], x: Vec3): Vec3 = xyz(
    m(0)(0) * x.x + m(0)(1) * x.y + m(0)(2) * x.z,
    m(1)(0) * x.x + m(1)(1) * x.y + m(1)(2) * x.z,
    m(2)(0) * x.x + m(2)(1) * x.y + m(2)(2) * x.z)

  private def rotationMatrix(axis: Vec3, angle: Float): List[List[Float]] = {
    val u: Vec3 = axis.unit
    val ux: Float = u.x
    val uy: Float = u.y
    val uz: Float = u.z
    val cosa: Float = cos(angle).asInstanceOf[Float]
    val sina: Float = sin(angle).asInstanceOf[Float]

    List(
      List(
        cosa + ux * ux * (1 - cosa),
        ux * uy * (1 - cosa) - uz * sina,
        ux * uz * (1 - cosa) + uy * sina
      ),
      List(
        uy * ux * (1 - cosa) + uz * sina,
        cosa + uy * uy * (1 - cosa),
        uy * uz * (1 - cosa) - ux * sina
      ),
      List(
        uz * ux * (1 - cosa) - uy * sina,
        uz * uy * (1 - cosa) + ux * sina,
        cosa + uz * uz * (1 - cosa)
      )
    )
  }

  def rotatePointAroundLine(line: Line3, c$: Vec3, angle: Float): Vec3 = {
    val matrix = rotationMatrix(line.ab, angle)
    var c = c$
    c = c.sub(line.a)
    c = matrixApply(matrix, c)
    c = c.add(line.a)
    c
  }

  trait Sphere {
    def center: Vec3
    def radius: Float
  }

  private class SimpleSphere (val _center: Vec3, val _radius: Float) extends Sphere {
    override def center = _center
    override def radius = _radius
  }

  def sphere(center: Vec3, radius: Float): Sphere =
    new SimpleSphere(center, radius)

  trait Circle3 {
    def center: Vec3

    /** A vector orthogonal to the plane on which the circle lies. */
    def normal: Vec3

    def radius: Float

    /** This sphere's intersection with the XY plane. 0 or 1 circles. */
    def intersectXY: Option[Circle2]

  }

  class SimpleCircle3 (
        val _center: Vec3, val _normal: Vec3, val _radius: Float
      ) extends Circle3 {

    override def center = _center
    override def normal = _normal
    override def radius = _radius

    override def intersectXY = {
      val r: Float = radius
      val d: Float = center.z
      if (d > r) None
      else Some(circle(center.xy, r * r - d * d))
    }

  }

  def circle(center: Vec3, normal: Vec3, radius: Float): Circle3 =
    new SimpleCircle3(center, normal, radius)

  /** 0 or 1 circles. */
  def intersect(a: Sphere, b: Sphere): Option[Circle3] = {
    val R: Float = a.radius
    val r: Float = b.radius
    if (distance(a.center, b.center) > R + r) {
      return None
    }
    val ab: Line3 = aToB(a.center, b.center)
    val d: Float = ab.mag
    val x: Float = (d * d - r * r + R * R) / (2 * d)
    val center: Vec3 = ab.a.add(ab.ab.mag(x))
    val rad: Float = sqrt(
      (r - d - R) * (R - d - r) * (r - d + R) * (d + r + R)
    ).toFloat / (2 * d)
    Some(circle(center, ab.ab, rad))
  }

  def parseXYZ(s: String): Vec3 = {
    val ss: Array[String] = s.split(",")
    xyz(
      java.lang.Float.parseFloat(ss(0)),
      java.lang.Float.parseFloat(ss(1)),
      java.lang.Float.parseFloat(ss(2))
    )
  }

  def formatXYZ(v: Vec3): String = "%f,%f,%f".format(v.x, v.y, v.z)

}
