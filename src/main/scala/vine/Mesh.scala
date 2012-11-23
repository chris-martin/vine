package vine

import vine.geometry.geometry3._
import collection.mutable.{ArrayBuffer,HashSet}

class Mesh() {

  private val _vertices = new ArrayBuffer[Vertex]
  private val _components = new HashSet[Component]()

  /** Number of vertices */
  def numVertices: Int = _vertices.size

  /** All triangles */
  def triangles: Iterable[Triangle] = _components.flatten

  /** Number of triangles */
  def numTriangles: Int = _components.map(c => c.size).sum

  /** Number of connected components */
  def numComponents: Int = _components.size

  /** All edges */
  def edges: Seq[Edge] = {
    val edges = new collection.mutable.HashSet[Edge]()
    for (t <- triangles)
      for (e <- t.undirectedEdges)
        edges.add(e)
    edges.toSeq
  }

  /** Number of edges */
  def numEdges: Int = edges.size

  /** Finds any triangles that contain edge e. */
  def findTriangles(e: Edge): Seq[Triangle] = {
    val found = HashSet[Triangle]()
    for (v <- e.vertices) {
      for (c <- v.corners) {
        val t = c.triangle
        val matchedEdge = t.directedEdge(e)
        if (matchedEdge.isDefined) {
          found add t
        }
      }
    }
    found.toSeq
  }

  /*
  def getBfsPseudoHamiltonianCycle: Iterable[Edge] = {

  }*/

  def shift(offset: Vec) { for (v <- _vertices) v.location = v.location + offset }

  def translateCenterToOrigin() { shift(center * -1) }

  def center = {
    var min = xyz(Float.MaxValue, Float.MaxValue, Float.MaxValue)
    var max = xyz(Float.MinValue, Float.MinValue, Float.MinValue)
    for (v <- _vertices) {
      min = xyz(math.min(min.x, v.location.x), math.min(min.y, v.location.y), math.min(min.z, v.location.z))
      max = xyz(math.max(max.x, v.location.x), math.max(max.y, v.location.y), math.max(max.z, v.location.z))
    }
    midpoint(min, max)
  }

  override def toString = "Mesh(%d vertices, %d components, %d triangles, %d edges)".format(
    numVertices, numComponents, numTriangles, numEdges)

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

  private class Component extends Iterable[Triangle] {

    _components add this

    val _id = Component.idGenerator.next()

    var isReversed: Boolean = false
    def toggleReverse() { isReversed = !isReversed }

    private val _triangles = new ArrayBuffer[Triangle]
    def iterator = _triangles.iterator
    def add(t: Triangle) { _triangles append t }

    def convertTo(o: Component) {
      for (t <- this) t setComponent o
      _components remove this
      _triangles clear()
    }

    override def toString = "Component %d".format(_id)

  }

  class Triangle (private val _corners:Array[Corner]) {

    private var component: Component = null

    def this(vertices:Seq[Vertex]) {

      this(new Array[Corner](3))

      for (i <- 0 until 3) {
        _corners(i) = new Corner(vertices(i), Triangle.this)
      }

      setComponent(new Component)

      for (myEdge:DirectedEdge <- directedEdges) {
        for (adjacentTriangle <- findTriangles(myEdge)) {
          val yourEdge:DirectedEdge = adjacentTriangle.directedEdge(myEdge).get

          if (component != adjacentTriangle.component) {

            val (smallerComponent, largerComponent) = {
              val a = component; val b = adjacentTriangle.component
              if (a.size < b.size) (a,b) else (b,a)
            }

            // If the shared edge between the two adjacent triangles is oriented
            // in the same direction for both triangles, then the two triangles are
            // in different components, and one of the components needs to be reversed.
            if (myEdge == yourEdge) { smallerComponent toggleReverse() }

            smallerComponent convertTo largerComponent

          }

          {
            val v = myEdge.vertices(0)
            val myCorner = cornerAtVertex(v).get
            val yourCorner = adjacentTriangle.cornerAtVertex(v).get
            myCorner.swing = yourCorner
          }
          {
            val v = myEdge.vertices(0)
            val myCorner = cornerAtVertex(v).get
            val yourCorner = adjacentTriangle.cornerAtVertex(v).get
            yourCorner.swing = myCorner
          }
        }
      }

    }

    def setComponent(c: Component) {
      this.component = c
      c add this
    }

    def corners: List[Corner] = component.isReversed match {
      case false => List.concat(_corners)
      case true => List.concat(_corners).reverse
    }

    def cornerAtVertex(v: Vertex): Option[Corner] = corners.find(c => c.vertex == v)

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

  class Vertex (var location:Vec) {

    val id = Vertex.idGenerator.next()

    _vertices.append(this)

    private var _corners:List[Corner] = List()
    def corners:List[Corner] = _corners
    private[Mesh] def addCorner(c: Corner) { _corners = _corners :+ c }

  }

  implicit def vertexToLocation(vertex:Vertex): Vec = vertex.location

}
