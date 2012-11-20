package vine

import vine.Geometry._
import collection.mutable.ArrayBuffer

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

  class Triangle private[Mesh] (val corners:Array[Corner]) {

    def this(vertices:Array[Vertex]) {
      this(new Array[Corner](3))
      for (i <- 0 until 3) {
        corners(i) = new Corner(vertices(i), Triangle.this)
      }
    }

    def vertexLocations = List(
      corners(0).vertex.location,
      corners(1).vertex.location,
      corners(2).vertex.location
    )

  }

  class Corner private[Mesh] (val vertex:Vertex, val triangle:Triangle) {
    if (vertex.corner.isEmpty) {
      vertex.corner = Some(this)
    }
  }

  class Vertex private[Mesh] (var location:Vec3) {
    private[Mesh] var corner = Option.empty[Corner]
  }

}
