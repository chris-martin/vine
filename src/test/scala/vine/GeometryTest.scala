package vine

import org.scalatest._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import math._
import vine.geometry._

@RunWith(classOf[JUnitRunner])
class GeometryTest extends FunSuite {

  val epsilon = pow(10, -6).toFloat

  class TestFloat (val a: Float) {
    def ~(b: Float):Boolean = { abs(a-b) < epsilon }
  }
  implicit def testFloat(a: Float) = new TestFloat(a)

  class TestVec2 (val a: Vec2) {
    def ~(b: Vec2):Boolean = { (a.x ~ b.x) & (a.y ~ b.y) }
  }
  implicit def testVec2(a: Vec2) = new TestVec2(a)

  class TestVec2List (val a: List[Vec2]) {
    def ~(b: List[Vec2]):Boolean = {
      if (a.length != b.length) return false
      for (v <- b) if (!contains(v)) return false
      true
    }
    def contains(u: Vec2):Boolean = {
      for (v <- a) if (u ~ v) return true
      false
    }
  }
  implicit def testVec2List(a: List[Vec2]) = new TestVec2List(a)

  implicit def doubleToFloat(a: Double) = a.toFloat

  // 2d arithmetic

  test("(1,2) + (5,11) = (6,13)") {
    assert((xy(1, 2) + xy(5, 11)) ~ xy(6, 13))
  }
  test("(1,2) - (5,11) = (-4,-9)") {
    assert((xy(1, 2) - xy(5, 11)) ~ xy(-4, -9))
  }
  test("(3,-4) * 5 = (15,-20)") {
    assert((xy(3, -4) * 5) ~ xy(15, -20))
  }
  test("(3,-4) * -2 = (-6,8)") {
    assert((xy(3, -4) * -2) ~ xy(-6, 8))
  }
  test("(1,1) * -1 = (-1,-1)") {
    assert((xy(1, 1) * -1) ~ xy(-1, -1))
  }

  // 2d dot product

  test("(2,6) dot (4,1.5) = 17") {
    assert((xy(2, 6) dot xy(4, 1.5f)) ~ 17)
  }
  test("(2,6) dot (0,0) = 0") {
    assert((xy(2, 6) dot origin2) ~ 0)
  }
  test("(0,0) dot (4,19) = 0") {
    assert((origin2 dot xy(4, 19)) ~ 0)
  }

  // 2d magnitude

  test("mag(3,4) = 5") {
    assert(xy(3,4).mag ~ 5)
  }
  test("mag(1,-1) = sqrt(2)") {
    assert(xy(1,-1).mag ~ sqrt(2))
  }

  // 2d angle

  test("ang(1,0) = 0") {
    assert(xy(1, 0).ang ~ 0)
  }
  test("ang(-1,0) = pi") {
    assert(xy(-1, 0).ang ~ Pi)
  }

  // Side of 2d line

  test("(1,1) on right side of (0,0)->(0,1)") {
    val point = xy(1, 1); val line = oTo2(xy(0, 1))
    assert((line side point) == Right)
  }
  test("(-1,1) on left side of (0,0)->(0,1)") {
    val point = xy(-1, 1); val line = oTo2(xy(0, 1))
    assert((line side point) == Left)
  }
  test("(1,1) on left side of (0,0)->(0,-1)") {
    val point = xy(1, 1); val line = oTo2(xy(0, -1))
    assert((line side point) == Left)
  }
  test("(-1,1) on right side of (0,0)->(0,-1)") {
    val point = xy(-1, 1); val line = oTo2(xy(0, -1))
    assert((line side point) == Right)
  }
  test("(1,0) on right side of (0,0)->(1,1)") {
    val point = xy(1, 0); val line = oTo2(xy(1, 1))
    assert((line side point) == Right)
  }
  test("(0,1) on left side of (0,0)->(1,1)") {
    val point = xy(0, 1); val line = oTo2(xy(1, 1))
    assert((line side point) == Left)
  }
  test("(-1000,1000) on left side of (10,10)->(14,11)") {
    val point = xy(-1000, 1000); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Left)
  }
  test("(1000,-1000) on right side of (10,10)->(14,11)") {
    val point = xy(1000, -1000); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Right)
  }
  test("(1000,0) on right side of (10,10)->(14,11)") {
    val point = xy(1000, 0); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Right)
  }
  test("(10,11) on left side of (10,10)->(14,11)") {
    val point = xy(10, 11); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Left)
  }
  test("(10,9) on right side of (10,10)->(14,11)") {
    val point = xy(10, 9); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Right)
  }
  test("(14,11.1) on left side of (10,10)->(14,11)") {
    val point = xy(14, 11.1f); val line = aToB(xy(10, 10), xy(14, 11))
    assert((line side point) == Left)
  }

  // 2d line angle

  test("ang((2,3)->(4,3)) = 0") {
    val line = aToB(xy(2, 3), xy(4, 3))
    assert(line.ang ~ 0)
  }

  // Line-line intersection

  test("((0,0)->(2,2)) intersects ((2,0)->(-1,3)) at (1,1)") {
    val line1 = aToB(xy(0, 0), xy(2, 2))
    val line2 = aToB(xy(2, 0), xy(-1, 3))
    assert(intersect(line1, line2) ~ xy(1, 1))
  }

  // Line-circle intersection

  test("((3,1)->(4,2)) intersects circle(c=(3,1),r=sqrt(2)) at (4,2) and (2,0)") {
    val line:Line2 = aToB(xy(3, 1), xy(4, 2))
    val circ:Circle2 = circle(xy(3, 1), sqrt(2))
    assert(intersect(line, circ) ~ List(xy(4, 2), xy(2, 0)))
  }

  // Triangle circumcenter

  test("circumcenter of triangle ((1,0),(0,2),(0,0)) is (.5,1)") {
    val circ = circle(xy(1, 0), xy(0, 2), xy(0, 0))
    assert(circ.center ~ xy(.5f, 1))
  }

  // Bulge

  test("bulge from line (0,0)->(1,0) to (.5,.1) is less than to (.5,.2)") {
    val line = aToB(xy(0, 0), xy(1, 0))
    assert(line.bulge(xy(.5f, .1f)) < line.bulge(xy(.5f, .2f)))
  }
  test("bulge from line (0,0)->(1,0) to (.5,-.1) is less than to (.5,-.2)") {
    val line = aToB(xy(0, 0), xy(1, 0))
    assert(line.bulge(xy(.5f, -.1f)) < line.bulge(xy(.5f, -.2f)))
  }
  test("bulge from line (0,0)->(1,0) to (.5,.1) is less than to (.5,20)") {
    val line = aToB(xy(0, 0), xy(1, 0))
    assert(line.bulge(xy(.5f, .1f)) < line.bulge(xy(.5f, 20)))
  }
  test("bulge from line (0,0)->(1,0) to (.5,10) is less than to (.5,20)") {
    val line = aToB(xy(0, 0), xy(1, 0))
    assert(line.bulge(xy(.5f, 10)) < line.bulge(xy(.5f, 20)))
  }
  test("bulge from line (660,28)->(707,113) to (119,563) is greater than 0") {
    val line = aToB(xy(660, 28), xy(707, 113))
    assert(line.bulge(xy(119, 563)) > 0)
  }

  // Azimuth and elevation

  test("(azimuth=0, elevation=0, mag=5).mag(-6) has mag 6") {
    val point = azimuthAndElevation(0, 0, 5).mag(-6)
    assert(point.mag ~ 6)
  }
  test("(azimuth=0, elevation=0, mag=5).mag(-6) has x value -6") {
    val point = azimuthAndElevation(0, 0, 5).mag(-6)
    assert(point.x ~ -6)
  }

}
