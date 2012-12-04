package vine.mesh

import collection.mutable
import vine.collection.{UndirectedAcyclicGraph, CircularSet}

abstract class Mesh {

  type Location

  private val _vertices = new mutable.ArrayBuffer[Vertex]
  private val _components = new mutable.HashSet[Component]

  def vertices: Iterable[Vertex] = _vertices
  def numVertices: Int = _vertices.size
  def addVertex(location: Location): Vertex = new Vertex(location)

  def triangles: Iterable[Triangle] = components.flatten
  def numTriangles: Int = _components.toSeq.map(c => c.size).sum
  def addTriangle(vs: Vertex*): Triangle = new Triangle(vs)

  def components: Iterable[Component] = _components
  def numComponents: Int = _components.size

  def edges: Seq[UndirectedEdge] = triangles.flatMap(t => t.undirectedEdges).toSeq
  def numEdges: Int = edges.size

  /** Finds any triangles that contain edge e. */
  def findTriangles(e: Edge): Seq[Triangle] =
    e.vertices.map(v => v.corners).flatten.map(c => c.triangle).
      filter(t => t.directedEdge(e).isDefined).toSeq.distinct

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

    if (_a < _b) { val t = _a; _a = _b; _b = t }

    override def vertices = List(_a, _b)
    override def undirectedEdge = this

    def canEqual(obj: Any): Boolean = obj.isInstanceOf[UndirectedEdge]

  }

  class DirectedEdge (private val _a: Vertex, private val _b: Vertex) extends Edge {

    override def vertices = List(_a, _b)
    override def undirectedEdge = new UndirectedEdge(_a, _b)

    def canEqual(obj: Any): Boolean = obj.isInstanceOf[DirectedEdge]

  }

  private object Component {
    private val idGenerator:Iterator[Int] = (1 until Int.MaxValue).iterator
  }

  class Component private[Mesh] () extends Iterable[Triangle] {

    _components add this

    private val _id = Component.idGenerator.next()

    private val _triangles = new mutable.ArrayBuffer[Triangle]
    def iterator = _triangles.iterator

    def corners: Iterable[Corner] = flatMap(_.corners)

    private[Mesh] def add(t: Triangle) { _triangles append t }

    private[Mesh] def reverse() { for (t <- this) t reverse() }

    private[Mesh] def convertTo(o: Component) {
      for (t <- this) t setComponent o
      _components remove this
      _triangles clear()
    }

    override def toString() = "Component #%d with %d triangles".format(_id, size)

  }

  object Triangle {
    val idGenerator:Iterator[Int] = (1 until Int.MaxValue).iterator
  }

  class Triangle private (private val _corners:Array[Corner]) {

    val id = Vertex.idGenerator.next()

    private var component: Component = null

    def this(vertices:Seq[Vertex]) {

      this(new Array[Corner](3))

      for (i <- 0 until 3) {
        _corners(i) = new Corner(vertices(i), Triangle.this)
      }

      setComponent(new Component())

      for (myEdge:DirectedEdge <- directedEdges) {
        for (adjacentTriangle <- findTriangles(myEdge).filter(t => t != Triangle.this)) {
          val yourEdge:DirectedEdge = adjacentTriangle.directedEdge(myEdge).get

          if (component != adjacentTriangle.component) {

            val (smallerComponent, largerComponent) = {
              val a = component; val b = adjacentTriangle.component
              if (a.size < b.size) (a,b) else (b,a)
            }

            // If the shared edge between the two adjacent triangles is oriented
            // in the same direction for both triangles, then the two triangles are
            // in different components, and one of the components needs to be reversed.
            if (myEdge == yourEdge) { smallerComponent reverse() }

            smallerComponent convertTo largerComponent

          }

          for (i <- 0 until 2) {
            val v = myEdge.vertices(i)
            var corners = ( cornerAtVertex(v).get, adjacentTriangle.cornerAtVertex(v).get )
            if (i == 1) corners = corners.swap
            corners._2.swing = Some(corners._1)
          }
        }
      }

    }

    def setComponent(c: Component) {
      this.component = c
      c add this
    }

    def reverse() { val t = _corners(0); _corners(0) = _corners(1); _corners(1) = t }

    def corners: Seq[Corner] = List.concat(_corners)

    def cornerAtVertex(v: Vertex): Option[Corner] = corners.find(c => c.vertex == v)

    def vertices = corners.map(corner => corner.vertex)

    def vertexLocations = vertices.map(vertex => vertex.location)

    def directedEdges: Seq[DirectedEdge] = corners.map(
      corner => new DirectedEdge(corner.vertex, corner.next.vertex))

    def undirectedEdges: Seq[UndirectedEdge] = corners.map(
      corner => new UndirectedEdge(corner.vertex, corner.next.vertex))

    def directedEdge(e: Edge): Option[DirectedEdge] =
      directedEdges.find(d => d.undirectedEdge == e.undirectedEdge)

    def sharedEdges(o: Triangle): Iterable[UndirectedEdge] =
      undirectedEdges.intersect(o.undirectedEdges)

    override def toString = "Triangle #%d".format(id)

  }

  class Corner private[Mesh] (val vertex:Vertex, val triangle:Triangle) {
    var swing: Option[Corner] = None
    vertex addCorner this
    def next: Corner = next(1)
    def prev: Corner = next(2)
    private def next(i: Int) = triangle.corners((triangle.corners.indexOf(this) + i) % 3)
    def opposite: Option[Corner] = prev.swing.map(c => c.prev)
    override def toString = "Corner of %s at %s".format(triangle, vertex)
  }

  object Vertex {
    val idGenerator:Iterator[Int] = (1 until Int.MaxValue).iterator
  }

  class Vertex private[Mesh] (var location: Location) extends Ordered[Vertex] {

    val id = Vertex.idGenerator.next()

    _vertices.append(this)

    private var _corners:List[Corner] = List()
    def corners:List[Corner] = _corners
    private[Mesh] def addCorner(c: Corner) { _corners = _corners :+ c }

    override def compare(that: Vertex) = id - that.id

    override def toString() = "Vertex #%d at %s".format(id, location)

  }

  implicit def vertexToLocation(vertex:Vertex): Location = vertex.location
  implicit def cornerToLocation(corner:Corner): Location = corner.vertex.location

  class LR ( val triangles: UndirectedAcyclicGraph[Triangle],
             val cycle: CircularSet[Vertex] )

  def arbitraryFirstCorner(component: Component) = component.head.corners.head

  def lr(firstCorner: Component => Corner = arbitraryFirstCorner): Seq[LR] =
    components.map(c => lr(firstCorner(c))).toSeq

  def lr(component: Component): LR = lr(arbitraryFirstCorner(component))

  def lr(firstCorner: Corner): LR = {

    val selectedTriangles = new UndirectedAcyclicGraph[Triangle]
    val visitedVertices = new mutable.HashSet[Vertex]
    val workCorners = new mutable.Queue[Corner]
    val cycle = new CircularSet[Vertex]

    def select(c: Corner, linkToOpposite: Boolean = true) {
      visitedVertices add c.vertex
      cycle insert (c.prev.vertex, c.vertex, c.next.vertex)
      workCorners enqueue c.next
      workCorners enqueue c.prev
      selectedTriangles add c.triangle
      if (linkToOpposite) {
        for (o <- c.opposite) {
          selectedTriangles.link(c.triangle, o.triangle)
        }
      }
    }

    workCorners enqueue firstCorner
    select(firstCorner, linkToOpposite = false)
    visitedVertices add firstCorner.next.vertex
    visitedVertices add firstCorner.prev.vertex

    while (workCorners.nonEmpty) {
      (
        workCorners.dequeue().opposite
          .filterNot(visitedVertices contains _.vertex)
          .foreach(select(_))
      )
    }

    new LR(selectedTriangles, cycle)
  }

}
