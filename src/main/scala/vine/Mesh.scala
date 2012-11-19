package vine

import vine.Geometry.Vec3
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

  class Triangle private[Mesh] (val corners:Array[Corner]) {
    def this(vertices:Array[Vertex]) {
      this(new Array[Corner](3))
      for (i <- 0 until 3) {
        corners(i) = new Corner(vertices(i), Triangle.this)
      }
    }
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
