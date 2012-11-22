package vine.geometry

import math._

package object geometry2 {

  def xy(x: Float, y: Float): Vec =
    if (abs(x) < EPSILON && abs(y) < EPSILON) origin
    else new XY(x, y)

  def xy(x: Number, y: Number): Vec = xy(x.floatValue, y.floatValue)

  def origin = Origin

  def angleVec(ang: Float, mag: Float): Vec =
    if (abs(mag) < EPSILON) origin
    else new Ang(mod2piFlip(ang, mag < 0), abs(mag))

  def angleVec(ang: Number, mag: Number): Vec =
    angleVec(ang.floatValue, mag.floatValue)

  def angleVec(ang: Float, mag: Number): Vec =
    angleVec(ang, mag.floatValue)

  def angleVec(ang: Number, mag: Float): Vec =
    angleVec(ang.floatValue, mag)

  def angleVecUnit(ang: Float): Vec = angleVec(ang, 1f)

  def angleVecUnit(ang: Number): Vec = angleVecUnit(ang.floatValue)

  def oTo(ang: Float): Line = new OriginLine(angleVecUnit(ang))
  def oTo(ang: Number): Line = new OriginLine(angleVecUnit(ang))
  def oTo(p: Vec): Line = new OriginLine(p)

  def aToB(a: Vec, b: Vec): Line = new AtoB(a, b)

  def pointAndStep(a: Vec, ab: Vec) = new PointAndDirection(a, ab)
  def pointAndStepUnit(a: Vec, ang: Float) = new PointAndDirection(a, angleVecUnit(ang))
  def pointAndStepUnit(a: Vec, ang: Number) = new PointAndDirection(a, angleVecUnit(ang))

  def distance(a: Vec, b: Vec): Float = (a - b).mag
  def midpoint(a: Vec, b: Vec): Vec = (a + b) / 2

  def overlap(ab: Line, cd: Line): Boolean = {
    val a = ab.a; val b = ab.b; val c = cd.a; val d = cd.b
    (ab.side(c) ne ab.side(d)) & (cd.side(a) ne cd.side(b))
  }

  def circle(center: Vec, radius: Number): Circle =
    new SimpleCircle(center, radius.floatValue)

  def circumscribeTriangle(a: Vec, b: Vec, c: Vec): Circle =
    new TriangleCircle2(Array[Vec](a, b, c))

  def intersectLines(ab: Line, cd: Line): Vec = {

    val v1 = ab.a; val v2 = ab.b; val v3 = cd.a; val v4 = cd.b
    val x1 = v1.x; val y1 = v1.y; val x2 = v2.x; val y2 = v2.y
    val x3 = v3.x; val y3 = v3.y; val x4 = v4.x; val y4 = v4.y

    // http://en.wikipedia.org/wiki/Line-line_intersection
    val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / d
    val y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / d

    xy(x, y)
  }

  /**
   * 0, 1, or 2 intersections.
   */
  def intersectLineCircle(line$: Line, circle: Circle): List[Vec] = {
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
    val is = List[Vec](xy(Ddy + qx, nDdx + qy), xy(Ddy - qx, nDdx - qy))
    is.map(i => (i / (pow(dr, 2))) + cc)
  }

  object AwtImplicits {

    import java.awt

    implicit def pointToVec2(p: awt.Point): Vec = xy(p.x, p.y)
    implicit def mouseEventToVec2(e: awt.event.MouseEvent): Vec = xy(e.getX, e.getY)
  }

}