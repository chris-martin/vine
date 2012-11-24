package vine.mesh

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class meshTest extends FunSuite {

  class TestMesh extends Mesh {

    override type Location = String

    val vertexByName = new collection.mutable.HashMap[String, Vertex]()

    implicit def v(s: String): Vertex =
      if (vertexByName contains s) vertexByName(s)
      else { val v = addVertex(s); vertexByName(s) = v; v }

  }

  test("Tet") {
    val mesh = new TestMesh()
    import mesh._

    addTriangle("a", "b", "c")
    addTriangle("b", "c", "d")
    addTriangle("c", "d", "a")
    addTriangle("d", "a", "b")

    assert(numComponents == 1)
    assert(numTriangles == 4)
    assert(numEdges == 6)
  }

  test("Triangles with non-manifold intersection") {
    val mesh = new TestMesh()
    import mesh._

    addTriangle("a", "b", "c")
    addTriangle("c", "d", "e")

    println(numTriangles)

    assert(numComponents == 2)
    assert(numTriangles == 2)
    assert(numEdges == 6)
  }

}
