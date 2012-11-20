package vine

import vine.geometry._
import collection.mutable.ArrayBuffer
import scala.collection
import collection.mutable

class Mesh() {

  private val vertices = new ArrayBuffer[Vertex]
  private val triangles = new ArrayBuffer[Triangle]()

  def addVertex(location:Vec3):Vertex = {
    val vertex = new Vertex(location)
    vertices.append(vertex)
    vertex
  }

  def addTriangle(x:Vertex, y:Vertex, z:Vertex):Triangle = {
    val triangle = new Triangle(Array(x, y, z))
    triangles.append(triangle)
    triangle
  }

  def getTriangles:Seq[Triangle] = triangles

  def getEdges:Iterable[Edge] = {
    val edges = new collection.mutable.HashSet[Edge]()
    for (t <- triangles) for (e <- t.undirectedEdges) edges.add(e)
    edges
  }

  def getBfsPseudoHamiltonianCycle:Iterable[Edge] = {

  }

  def shift(offset:Vec3) {
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
    def vertices:List[Vertex]
    def locations = vertices.map(vertex => vertex.location)
  }

  case class UndirectedEdge private[Mesh] (var _a: Vertex, var _b: Vertex) extends Edge {
    if (_a.id < _b.id) { var t = _a; _a = _b; _b = t }
    override def vertices = List(_a, _b)
  }

  case class DirectedEdge private[Mesh] (_a: Vertex, _b: Vertex) extends Edge {
    override def vertices = List(_a, _b)
  }

  class Triangle private[Mesh] (private val _corners:Array[Corner]) {

    def this(vertices:Array[Vertex]) {
      this(new Array[Corner](3))
      for (i <- 0 until 3) {
        _corners(i) = new Corner(vertices(i), Triangle.this)
      }
    }

    def corners:List[Corner] = List.concat(_corners)
    def vertices = corners.map(corner => corner.vertex)
    def vertexLocations = vertices.map(vertex => vertex.location)
    def undirectedEdges:List[UndirectedEdge] = corners.map(
      corner => UndirectedEdge(corner.vertex, corner.next.vertex))

  }

  class Corner private[Mesh] (val vertex:Vertex, val triangle:Triangle) {
    vertex addCorner this
    def next:Corner = triangle.corners((triangle.corners.indexOf(this) + 1) % 3)

  }

  var nextVertexId = 0

  class Vertex private[Mesh] (var location:Vec3) {
    val id = nextVertexId; nextVertexId += 1
    private val corners = new ArrayBuffer[Corner]
    private[Mesh] def addCorner(c: Corner) { corners append c; sortCorners }
    private def sortCorners {
      val swing = new mutable.HashMap[Corner, Corner]
      for (a <- corners) for (b <- corners
    }
  }

  object implicits {
    implicit def vertexToLocation(vertex:Vertex): Vec3 = vertex.location
  }

}
