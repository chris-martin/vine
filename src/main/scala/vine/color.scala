package vine

import java.nio.FloatBuffer
import util.parsing.combinator.RegexParsers

object color {

  trait Color {

    def r: Float
    def g: Float
    def b: Float
    def a: Float

    def toArray: Array[Float] = Array(r, g, b, a)
    def toFloatBuffer: FloatBuffer = FloatBuffer.wrap(toArray)

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

  class RGBA (val r:Float, val g:Float, val b:Float, val a:Float) extends Color

  class RGB (val r:Float, val g:Float, val b:Float) extends Color {
    override def a = 1
  }

  val white = new RGB(1, 1, 1)
  val black = new RGB(0, 0, 0)

  val transparent = white.transparent

  def apply(s: String): Color = parser.parse(s).get

  def fromList(xs: List[Float]) = {
    if (xs.length == 3) new RGB(xs(0), xs(1), xs(2))
    else if (xs.length == 4) new RGBA(xs(0), xs(1), xs(2), xs(3))
    else throw new IllegalArgumentException
  }

  object implicits {
    implicit def toArray(c: Color): Array[Float] = c.toArray
    implicit def toFloatBuffer(c: Color): FloatBuffer = c.toFloatBuffer
  }

  object parser extends RegexParsers {

    def parse(s: String): Option[Color] = parseAll(phrase, s.toLowerCase).map(Some(_)).getOrElse(None)

    def phrase = longHexRGBA | longHexRGB | shortHexRGBA | shortHexRGB | transparent

    def longHexRGBA = (longHexRGB ~ longHex) ^^ { case rgb ~ a => rgb withAlpha a }
    def longHexRGB = ("#" ~> repN(3, longHex)) ^^ color.fromList
    def longHex = repN(2, "[0-9a-f]".r) ^^ { xs => (16*parseHex(xs(0)) + parseHex(xs(1))) / 255f }

    def shortHexRGBA = (shortHexRGB ~ shortHex) ^^ { case rgb ~ a => rgb withAlpha a }
    def shortHexRGB = ("#" ~> repN(3, shortHex)) ^^ color.fromList
    def shortHex = ("[0-9a-f]".r) ^^ { x => parseHex(x) / 15f }

    def transparent = "transparent" ^^ { _ => color.transparent }

    def parseHex(s: String) = Integer.parseInt(s, 16)

  }

}
