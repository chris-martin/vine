package vine

package object color {

  class RGBA (val r:Float, val g:Float, val b:Float, val a:Float) extends Color

  class RGB (val r:Float, val g:Float, val b:Float) extends Color {
    override def a = 1
  }

  val white = new RGB(1, 1, 1)
  val black = new RGB(0, 0, 0)

  val transparent = white.transparent

  def parse(s: String): Color = Parser.parse(s).get

  def fromList(xs: List[Float]) = {
    if (xs.length == 3) new RGB(xs(0), xs(1), xs(2))
    else if (xs.length == 4) new RGBA(xs(0), xs(1), xs(2), xs(3))
    else throw new IllegalArgumentException
  }

  implicit def toArray(c: Color): Array[Float] = c.toArray
  implicit def toFloatBuffer(c: Color) = c.toFloatBuffer

}
