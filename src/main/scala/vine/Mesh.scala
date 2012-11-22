package vine

import vine.geometry._
import collection.mutable.ArrayBuffer

class Mesh() {

  private val vertices = new ArrayBuffer[Vertex]
  private val triangles = new ArrayBuffer[Triangle]()

  def getTriangles: Seq[Triangle] = triangles

  def getEdges: Iterable[Edge] = {
    val edges = new collection.mutable.HashSet[Edge]()
    for (t <- triangles) for (e <- t.undirectedEdges) edges.add(e)
    edges
  }
  /*
  def findTriangles(e: UndirectedEdge): List[Triangle] = {
    var found = List()
    for (v <- e.vertices) {
      for (c <- v.corners) {
        val matchedEdge = c.triangle.directedEdge(e)

      }
    }
    found
  }*/
  /*
  def getBfsPseudoHamiltonianCycle: Iterable[Edge] = {

  }*/

  def shift(offset: Vec3) {
    for (v <- vertices) {
      v.location = v.location.add(offset)
    }
  }

  def center() {
    var min = xyz(Float.MaxValue, Float.MaxValue, Float.MaxValue)
    var max = xyz(Float.MinValue, Float.MinValue, Float.MinValue)
    for (v <- vertices) {
      min = xyz(math.min(min.x, v.location.x), math.min(min.y, v.location.y), math.min(min.z, v.location.z))
      max = xyz(math.max(max.x, v.location.x), math.max(max.y, v.location.y), math.max(max.z, v.location.z))
    }
    shift(midpoint(min, max).mult(-1))
  }

  override def toString = "Mesh(%d vertices, %d triangles, %d edges)".format(
    vertices.length, triangles.length, getEdges.size)

  trait Edge {

    def vertices: List[Vertex]
    def locations = vertices.map(vertex => vertex.location)
    def undirectedEdge: UndirectedEdge
    def canEqual(obj: Any): Boolean

    override def equals(obj: Any) = obj match {
      case that: Edge => (that canEqual this) && that.vertices == vertices
      case _ => false
    }
    override def hashCode() = vertices.hashCode()

  }

  class UndirectedEdge private[Mesh] (private var _a: Vertex, private var _b: Vertex) extends Edge {

    if (_a.id < _b.id) { val t = _a; _a = _b; _b = t }

    override def vertices = List(_a, _b)
    override def undirectedEdge = this

    def canEqual(obj: Any): Boolean = obj.isInstanceOf[UndirectedEdge]

  }

  class DirectedEdge private[Mesh] (private val _a: Vertex, private val _b: Vertex) extends Edge {

    override def vertices = List(_a, _b)
    override def undirectedEdge = new UndirectedEdge(_a, _b)

    def canEqual(obj: Any): Boolean = obj.isInstanceOf[DirectedEdge]

  }

  private object Component {
    private val idGenerator:Iterator[Int] = (1 until Int.MaxValue).iterator
  }

  private class Component (var size:Int) {
    val id = Component.idGenerator.next()
  }

  class Triangle (private val _corners:Array[Corner]) {

    private var component = new Component(1)

    triangles.append(this)

    def this(vertices:Seq[Vertex]) {
      this(new Array[Corner](3))
      for (i <- 0 until 3) {
        _corners(i) = new Corner(vertices(i), Triangle.this)
      }
      /*for (e <- undirectedEdges) {
        for (adjacentTriangle <- findTriangles(e)) {
          val de = adjacentTriangle.directedEdge(e)
        }
      }*/
    }

    def corners:List[Corner] = List.concat(_corners)

    def vertices = corners.map(corner => corner.vertex)

    def vertexLocations = vertices.map(vertex => vertex.location)

    def directedEdges: List[DirectedEdge] = corners.map(
      corner => new DirectedEdge(corner.vertex, corner.next.vertex))

    def undirectedEdges: List[UndirectedEdge] = corners.map(
      corner => new UndirectedEdge(corner.vertex, corner.next.vertex))

    def directedEdge(e: Edge): Option[DirectedEdge] =
      directedEdges.find(d => d.undirectedEdge == e.undirectedEdge)

  }

  class Corner private[Mesh] (val vertex:Vertex, val triangle:Triangle) {

    var swing:Corner = this

    vertex addCorner this

    def next:Corner = triangle.corners((triangle.corners.indexOf(this) + 1) % 3)

  }

  object Vertex {
    val idGenerator:Iterator[Int] = (1 until Int.MaxValue).iterator
  }

  class Vertex (var location:Vec3) {

    val id = Vertex.idGenerator.next()

    vertices.append(this)

    private var _corners:List[Corner] = List()
    def corners:List[Corner] = _corners
    private[Mesh] def addCorner(c: Corner) { _corners = _corners :+ c }

  }

  implicit def vertexToLocation(vertex:Vertex): Vec3 = vertex.location

}
