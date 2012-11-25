package vine.geometry.geometry3

import org.scalatest._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vine.geometry.geometryTest._

@RunWith(classOf[JUnitRunner])
class geometry3Test extends FunSuite {

  // Azimuth and elevation

  test("(azimuth=0, elevation=0, mag=5).mag(-6) has mag 6") {
    val point = azimuthAndElevation(0, 0, 5).mag(-6)
    assert(point.mag ~ 6)
  }
  test("(azimuth=0, elevation=0, mag=5).mag(-6) has x value -6") {
    val point = azimuthAndElevation(0, 0, 5).mag(-6)
    assert(point.x ~ -6)
  }
  test("azimuth of (1,0,0) is 0") {
    assert(xyz(1, 0, 0).azimuth == 0)
  }
  test("elevation of (1,0,0) is 0") {
    assert(xyz(1, 0, 0).elevation == 0)
  }
  test("azimuth of (0,1,0) is pi/2") {
    assert(xyz(0, 1, 0).azimuth ~ (math.Pi / 2).toFloat)
  }
  test("elevation of (0,1,0) is 0") {
    assert(xyz(0, 1, 0).elevation == 0)
  }
  test("elevation of (0,0,1) is pi/2") {
    assert(xyz(0, 0, 1).elevation ~ (math.Pi / 2).toFloat)
  }
  test("elevation of (0,0,-1) is -pi/2") {
    assert(xyz(0, 0, -1).elevation ~ (math.Pi / -2).toFloat)
  }
  test("elevation of (0,1,1) is pi/4") {
    assert(xyz(0, 1, 1).elevation ~ (math.Pi / 4).toFloat)
  }

}
