package vine.geometry

import math._

package object geometry3 {

  private[geometry3] def elevation(ang: Float): Float = {
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

  def xyz(x: Float, y: Float, z: Float): Vec =
    if (abs(x) < EPSILON && abs(y) < EPSILON && abs(z) < EPSILON) origin
    else new XYZ(x, y, z)

  def xyz(x: Number, y: Number, z: Number): Vec =
    xyz(x.floatValue, y.floatValue, z.floatValue)

  implicit def xyzSeq(seq: Seq[Float]): Vec = xyz(seq(0), seq(1), seq(2))
  implicit def xyzArray(array: Array[Float]): Vec = xyzSeq(array toSeq)

  def origin = Origin

  def distance(a: Vec, b: Vec): Float = (a - b).mag
  def midpoint(a: Vec, b: Vec): Vec = (a + b) / 2

  def aToB(a: Vec, b: Vec): Line = new AtoB(a, b)

  /**
   * Start with (1, 0, 0).
   * Rotate by the azimuth angle on the XY plane.
   * Rotate toward the Z axis by the elevation angle.
   */
  def azimuthAndElevation(azimuth: Float, elevation: Float, mag: Float): Vec =
    if (abs(mag) < EPSILON) origin
    else new AzimuthAndElevation(
      mod2piFlip(azimuth, mag < 0),
      geometry3.elevation(elevation) * sign(mag),
      abs(mag)
    )

  def pointAndStep(a: Vec, ab: Vec): Line =
    new PointAndDirection(a, ab)

  def oTo3(b: Vec): Line = aToB(origin, b)

  def sphere(center: Vec, radius: Float): Sphere =
    new SimpleSphere(center, radius)

  def circle(center: Vec, normal: Vec, radius: Float): Circle =
    new SimpleCircle(center, normal, radius)

  def distance(line: Line, c: Vec): Float = {
    val a = line.a
    val b = line.b
    val ac = aToB(a, c).ab
    val bc = aToB(b, c).ab
    val ab = line.ab
    ac.cross(bc).mag / ab.mag
  }

  private[geometry3] def matrixApply(m: List[List[Float]], x: Vec): Vec = xyz(
    m(0)(0) * x.x + m(0)(1) * x.y + m(0)(2) * x.z,
    m(1)(0) * x.x + m(1)(1) * x.y + m(1)(2) * x.z,
    m(2)(0) * x.x + m(2)(1) * x.y + m(2)(2) * x.z)

  private[geometry3] def rotationMatrix(axis: Vec, angle: Float): List[List[Float]] = {
    val u: Vec = axis.unit
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

  def rotatePointAroundLine(line: Line, c$: Vec, angle: Float): Vec = {
    val matrix = rotationMatrix(line.ab, angle)
    var c = c$
    c = c.-(line.a)
    c = matrixApply(matrix, c)
    c = c.+(line.a)
    c
  }

  /**
   * 0 or 1 circles.
   */
  def intersect(a: Sphere, b: Sphere): Option[Circle] = {
    val R: Float = a.radius
    val r: Float = b.radius
    if (distance(a.center, b.center) > R + r) {
      return None
    }
    val ab: Line = aToB(a.center, b.center)
    val d: Float = ab.mag
    val x: Float = (d * d - r * r + R * R) / (2 * d)
    val center: Vec = ab.a.+(ab.ab.mag(x))
    val rad: Float = sqrt(
      (r - d - R) * (R - d - r) * (r - d + R) * (d + r + R)
    ).toFloat / (2 * d)
    Some(circle(center, ab.ab, rad))
  }

  def parseXYZ(s: String): Vec = {
    val ss: Array[String] = s.split(",")
    xyz(
      java.lang.Float.parseFloat(ss(0)),
      java.lang.Float.parseFloat(ss(1)),
      java.lang.Float.parseFloat(ss(2))
    )
  }

  def formatXYZ(v: Vec): String = "%f,%f,%f".format(v.x, v.y, v.z)

}
