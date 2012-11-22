package vine.color

import vine.color

object Parser extends util.parsing.combinator.RegexParsers {

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
