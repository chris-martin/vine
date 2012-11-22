package vine

object Ply {

  private val elementRegex = new util.matching.Regex(
    """element (\S+) ([-\.\d]+)""", "element", "count")
  private val whitespaceRegex = """[ \t]+""".r

  def parse(in: java.io.InputStream): Mesh = {

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

    val mesh = new Mesh()

    def nextSplitLine() = whitespaceRegex split(scanner next())

    val vertices = collection.mutable.ArrayBuffer[mesh.Vertex]()
    for (i <- 0 until (counts get "vertex" get)) {
      vertices append new mesh.Vertex( nextSplitLine() slice(0, 3) map { x => x.toFloat } )
    }

    for (i <- 0 until (counts get "face" get)) {
      new mesh.Triangle( nextSplitLine() slice(1, 4) map { x => vertices(x.toInt) } )
    }

    mesh
  }

}
