package vine

import math._

package object geometry {

  private[geometry] val EPSILON: Float = 0.000001f
  private[geometry] val PI: Float = Pi.toFloat
  private[geometry] val PI2: Float = PI * 2
  private[geometry] val HALFPI: Float = PI / 2
  private[geometry] val NEG_HALFPI: Float = PI / -2
  private[geometry] val THREE_HALVES_PI: Float = 1.5f * PI

  private[geometry] def mod2pi(a: Float): Float =
    if (a < 0) ((a % PI2) + PI2) else (a % PI2)

  private[geometry] def mod2piFlip(a: Float, flip: Boolean): Float =
    mod2pi(if (flip) a + PI else a)

  private[geometry] def sign(x: Float): Int =
    if (x == 0) 0 else (if (x < 0) -1 else 1)

}
