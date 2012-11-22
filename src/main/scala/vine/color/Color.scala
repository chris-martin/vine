package vine.color

trait Color {

  def r: Float
  def g: Float
  def b: Float
  def a: Float

  def toArray: Array[Float] = Array(r, g, b, a)
  def toFloatBuffer = java.nio.FloatBuffer.wrap(toArray)

  def withAlpha(a: Float) = new RGBA(r, g, b, a)
  def transparent = this withAlpha 0

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Color]) return false
    val c = obj.asInstanceOf[Color]
    r == c.r & g == c.g & b == c.b & a == c.a
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(toArray)

  override def toString = "R=%.3f, G=%.3f, B=%.3f, A=%.3f".format(r, g, b, a)

}
