package vine.geometry.geometry2

import math._
import vine.geometry._

/**
 * A point in a Euclidean plane.
 */
trait Vec extends Comparable[Vec] {

  def x: Float
  def y: Float
  def ang: Float
  def mag: Float
  def mag(newMag: Float): Vec = angleVec(ang, newMag)

  /** Equivalent to mag(1). */
  def unit: Vec = angleVec(ang, 1)

  def +(o: Vec): Vec = xy(x + o.x, y + o.y)
  def -(o: Vec): Vec = xy(x - o.x, y - o.y)
  def *(factor: Float): Vec
  def *(factor: Number): Vec = *(factor.floatValue)
  def /(divisor: Float): Vec
  def /(divisor: Number): Vec = /(divisor.floatValue)
  def rot(ang: Float): Vec = angleVec(ang + ang, mag)
  def rot(ang: Number): Vec = rot(ang.floatValue)
  def addX($: Float): Vec = xy(x + $, y)
  def addY($: Float): Vec = xy(x, y + $)
  def subX($: Float): Vec = xy(x - $, y)
  def subY($: Float): Vec = xy(x, y - $)

  /** This is exactly (0, 0)? */
  def isOrigin: Boolean = false

  def rot90: Vec
  def rot180: Vec
  def rot270: Vec

  /** Scalar (dot) product. */
  def dot(o: Vec): Float = x * o.x + y * o.y

  /** U x V = U dot rot90(V). */
  def cross(o: Vec): Float = dot(o.rot90)

  /** Raises this 2d vector into three dimensions by adding a zero-value z value. */
  def in3d: geometry3.Vec

  def compareTo(o: Vec): Int = java.lang.Float.compare(mag, o.mag)

  override def toString: String = "(%f, %f)".format(x, y)

}

private[geometry2] class XY (val _x: Float, val _y: Float) extends Vec {

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
    if (abs(f) < EPSILON) origin else xy(f * x, f * y)

  override def /(d: Float): Vec = xy(x / d, y / d)

  override def in3d: geometry3.Vec = geometry3.xyz(x, y, 0)

}

private[geometry2] class Ang (val _ang: Float, val _mag: Float) extends Vec {

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

  override def rot180 = angleVec(ang + PI, mag)
  override def rot90 = angleVec(ang + HALFPI, mag)
  override def rot270 = angleVec(ang - HALFPI, mag)

  override def *(f: Float) =
    if (abs(f) < EPSILON) origin else angleVec(ang, f * mag)

  override def /(d: Float) = angleVec(ang, mag / d)

  override def in3d = geometry3.azimuthAndElevation(ang, 0, mag)

}

private[geometry2] object Origin extends Vec {

  override def x = 0
  override def y = 0
  override def ang = 0
  override def mag = 0
  override def *(factor: Float) = this
  override def /(divisor: Float) = this
  override def rot90 = this
  override def rot270 = this
  override def rot180 = this
  override def +(o: Vec) = o
  override def -(o: Vec) = o * -1
  override def addX($: Float) = xy($, 0)
  override def addY($: Float) = xy(0, $)
  override def subX($: Float) = xy(-$, 0)
  override def subY($: Float) = xy(0, -$)
  override def mag(newMag: Float) = this
  override def unit = this
  override def *(factor: Number) = this
  override def /(divisor: Number) = this
  override def dot(o: Vec) = 0
  override def cross(o: Vec) = 0
  override def rot(ang: Float) = this
  override def rot(ang: Number) = this
  override def compareTo(o: Vec) = java.lang.Float.compare(0, o.mag)
  override def isOrigin: Boolean = true
  override def in3d: geometry3.Vec = geometry3.origin
  override def toString: String = "(0, 0)"

}
