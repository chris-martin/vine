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

}
