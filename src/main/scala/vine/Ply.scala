package vine

import java.io.InputStream
import java.util.Scanner
import util.matching.Regex
import util.Properties.lineSeparator
import collection.mutable.ArrayBuffer

object Ply {

  val elementRegex = new Regex("""element (\S+) ([-\.\d]+)""", "element", "count")

  val whitespaceRegex = """[ \t]+""".r

  def parse(in:InputStream):Mesh = {
    val scanner = new Scanner(in)
    scanner.useDelimiter(lineSeparator)
    var line = ""
    val counts = new collection.mutable.HashMap[String, Int]()
    do {
      line = scanner.next()
      elementRegex findFirstIn line match {
        case Some(elementRegex(element, count)) => counts.put(element, Integer.parseInt(count))
        case None => { }
      }
    } while (!line.equals("end_header"))

    val mesh = new Mesh()

    val vertices = ArrayBuffer[mesh.Vertex]()
    for (i <- 0 until (counts get "vertex" get)) {
      line = scanner.next()
      val tokens:Array[String] = whitespaceRegex.split(line)
      vertices.append(mesh.addVertex(geometry.xyz(
        java.lang.Float.parseFloat(tokens{0}),
        java.lang.Float.parseFloat(tokens{1}),
        java.lang.Float.parseFloat(tokens{2})
      )))
    }

    for (i <- 0 until (counts get "face" get)) {
      line = scanner.next()
      val tokens:Array[String] = whitespaceRegex.split(line)
      mesh.addTriangle(
        vertices{Integer.parseInt(tokens{1})},
        vertices{Integer.parseInt(tokens{2})},
        vertices{Integer.parseInt(tokens{3})}
      )
    }

    mesh
  }

}
