package vine.geometry.geometry3

import vine.geometry._
import math._

/**
 * A point in three dimensions.
 */
trait Vec extends Comparable[Vec] {

  def x: Float
  def y: Float
  def z: Float

  def x(x: Float): Vec = xyz(x, y, z)
  def y(y: Float): Vec = xyz(x, y, z)
  def z(z: Float): Vec = xyz(x, y, z)

  def mag: Float
  def magSquared: Float
  def mag(newMag: Float): Vec = unit * newMag

  /**
   * Equivalent to mag(1).
   */
  def unit: Vec = this / mag

  def +(o: Vec): Vec = xyz(x + o.x, y + o.y, z + o.z)
  def -(o: Vec): Vec = xyz(x - o.x, y - o.y, z - o.z)
  def *(factor: Float): Vec
  def *(factor: Number): Vec = *(factor.floatValue)
  def /(divisor: Float): Vec
  def /(divisor: Number): Vec = /(divisor.floatValue)
  def addX($: Float): Vec = xyz(x + $, y, z)
  def addY($: Float): Vec = xyz(x, y + $, z)
  def addZ($: Float): Vec = xyz(x, y, z + $)
  def subX($: Float): Vec = xyz(x - $, y, z)
  def subY($: Float): Vec = xyz(x, y - $, z)
  def subZ($: Float): Vec = xyz(x, y, z - $)

  /**
   * This is exactly (0, 0, 0)?
   */
  def isOrigin: Boolean = false

  /**
   * Scalar (dot) product.
   */
  def dot(o: Vec): Float = x * o.x + y * o.y + z * o.z

  /**
   * Cross product U X V, normal to both U and V.
   */
  def cross(o: Vec): Vec =
    xyz(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)

  /** Rotate 90 degrees in the XY plane. */
  def rot90xy: Vec

  def azimuth: Float
  def elevation: Float

  def azimuth(newAzimuth: Float): Vec =
    azimuthAndElevation(newAzimuth, elevation, mag)

  def elevation(newElevation: Float): Vec =
    azimuthAndElevation(azimuth, newElevation, mag)

  def xy: geometry2.Vec

  /** An arbitrary orthogonal vector. */
  def orthog: Vec = azimuthAndElevation(azimuth, elevation + HALFPI, mag)

  def compareTo(o: Vec) = java.lang.Float.compare(mag, o.mag)

  override def toString: String = "(%f, %f, %f)".format(x, y, z)

}

private[geometry3] class XYZ
    (val _x: Float, val _y: Float, val _z: Float) extends Vec {

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

  override def *(f: Float) =
    if (abs(f) < EPSILON) origin else xyz(f*x, f*y, f*z)

  override def /(d: Float) = xyz(x/d, y/d, z/d)

  override def rot90xy = xyz(-1*y, x, z)

  override def elevation = {
    if (_elevation.isEmpty) {
      _elevation = Some(geometry3.elevation(asin(z / mag).toFloat))
    }
    _elevation.get
  }

  override def azimuth: Float = {
    if (_azimuth.isEmpty) {
      _azimuth = Some(mod2pi(atan2(y, x).toFloat))
    }
    _azimuth.get
  }

  override def xy = geometry2.xy(x, y)

}

private[geometry3] class AzimuthAndElevation
    ( val _azimuth: Float, val _elevation: Float, val _mag: Float) extends Vec {

  override def azimuth = _azimuth
  override def elevation = _elevation
  override def mag = _mag

  var _xyz: Option[Vec] = None

  private def xyz: Vec = {
    if (_xyz.isEmpty) {
      val cosAzimuth: Double = cos(azimuth)
      val cosElevation: Double = cos(elevation)
      val sinAzimuth: Double = sin(azimuth)
      val sinElevation: Double = sin(elevation)
      _xyz = Some(geometry3.xyz(
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
  override def *(factor: Float) =
    azimuthAndElevation(azimuth, elevation, mag * factor)
  override def /(divisor: Float) =
    azimuthAndElevation(azimuth, elevation, mag / divisor)
  override def rot90xy = xyz.rot90xy
  override def xy = geometry2.angleVec(azimuth, mag)

}

private[geometry3] object Origin extends Vec {

  override def x = 0
  override def y = 0
  override def z = 0
  override def mag = 0
  override def magSquared = 0
  override def *(factor: Float) = this
  override def /(divisor: Float) = this
  override def +(o: Vec) = o
  override def -(o: Vec) = o * -1
  override def addX($: Float) = xyz($, 0, 0)
  override def addY($: Float) = xyz(0, $, 0)
  override def addZ($: Float) = xyz(0, 0, $)
  override def subX($: Float) = xyz(-$, 0, 0)
  override def subY($: Float) = xyz(0, -$, 0)
  override def subZ($: Float) = xyz(0, 0, -$)
  override def mag(newMag: Float) = this
  override def unit = this
  override def *(factor: Number) = this
  override def /(divisor: Number) = this
  override def dot(o: Vec) = 0
  override def cross(o: Vec) = this
  override def rot90xy = this
  override def compareTo(o: Vec) = java.lang.Float.compare(0, o.mag)
  override def isOrigin = true
  override def azimuth = 0
  override def elevation = 0
  override def azimuth(newAzimuth: Float) = this
  override def elevation(newElevation: Float) = this
  override def xy = geometry2.origin
  override def orthog = this
  override def toString = "(0, 0, 0)"

}