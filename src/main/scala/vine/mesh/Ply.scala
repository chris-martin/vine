package vine.mesh

import vine.geometry.geometry3._

object Ply {

  private val elementRegex = new util.matching.Regex(
    """element (\S+) ([-\.\d]+)""", "element", "count")
  private val whitespaceRegex = """[ \t]+""".r

  def parse(in: java.io.InputStream): Mesh3d = {

    val scanner = new java.util.Scanner(in)
    scanner useDelimiter(util.Properties.lineSeparator)
    var line = ""
    val counts = new collection.mutable.HashMap[String, Int]
    do {
      line = scanner next()
      elementRegex findFirstIn line match {
        case Some(elementRegex(element, count)) => counts put(element, count.toInt)
        case None => { }
      }
    } while (!(line equals "end_header"))

    val mesh = new Mesh3d

    def nextSplitLine() = whitespaceRegex split(scanner next())

    val vertices = collection.mutable.ArrayBuffer[mesh.Vertex]()
    for (i <- 0 until (counts get "vertex" get)) {
      vertices append mesh.addVertex( nextSplitLine() slice(0, 3) map { x => x.toFloat } )
    }

    for (i <- 0 until (counts get "face" get)) {
      mesh addTriangle ( nextSplitLine() slice(1, 4) map { x => vertices(x.toInt) }:_* )
    }

    mesh
  }

}
