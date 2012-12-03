package vine.collection

import collection.mutable

class UndirectedAcyclicGraph[A] extends Iterable[A] {

  private val adjacencies =
    new mutable.HashMap[A, mutable.Set[A]] with MultiMapInsertion[A, A]

  override def iterator = adjacencies.keysIterator

  def add(a: A) {
    adjacencies ensureContains a
  }

  def link(a: A, b: A) {
    adjacencies addBinding (a, b)
    adjacencies addBinding (b, a)
  }

  def toTree(isRoot: A => Boolean): Forest[A] = {
    val forest = new Forest[A]()
    find(isRoot) foreach { root =>
      val work = new mutable.ArrayStack[A]()
      work push root
      val visited = mutable.HashSet[A]()
      while (work.nonEmpty) {
        val a = work pop()
        forest add a
        for (b <- adjacencies(a) filterNot { visited contains _ }) {
          work push b
          visited add b
          forest.add(parent=a, child=b)
        }
      }
    }
    forest
  }

}
