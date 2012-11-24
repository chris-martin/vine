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

    assert(numComponents == 2)
    assert(numTriangles == 2)
    assert(numEdges == 6)
  }

  test("Joining components") {
    val mesh = new TestMesh()
    import mesh._

    // two triangles with a non-manifold intersection
    addTriangle("a", "b", "c")
    addTriangle("a", "d", "e")

    // drop in a third triangle adjacent to both of them
    addTriangle("a", "b", "d")

    assert(numComponents == 1)
    assert(numTriangles == 3)
    assert(numEdges == 7)
  }

  test("Opposite") {
    val mesh = new TestMesh()
    import mesh._

    addTriangle("a", "b", "c")
    addTriangle("b", "c", "d")

    assert(v("a").corners(0).opposite.get.vertex eq v("d"))
  }

  test("Opposite (with one triangle reversed)") {
    val mesh = new TestMesh()
    import mesh._

    addTriangle("a", "b", "c")
    addTriangle("b", "d", "c")

    assert(v("a").corners(0).opposite.get.vertex eq v("d"))
  }

}
