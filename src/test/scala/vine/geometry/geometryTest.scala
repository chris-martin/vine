package vine.geometry

import math._

object geometryTest {

  val epsilon = pow(10, -6).toFloat

  class TestFloat (val a: Float) {
    def ~(b: Float):Boolean = { abs(a-b) < epsilon }
  }
  implicit def testFloat(a: Float) = new TestFloat(a)

  implicit def doubleToFloat(a: Double) = a.toFloat

}
