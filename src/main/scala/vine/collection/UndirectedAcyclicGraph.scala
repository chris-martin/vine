package vine.collection

import collection.mutable

/**
 * Assumptions that we make but do NOT check in this class:
 * - The graph is acyclic.
 * - The graph is a single connected component,
 */
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

  def toTree(isRootCandidate: A => Boolean): Forest[A] = {
    val forest = new Forest[A]()
    println("# " + size)
    for (root <- find(isRootCandidate)) {
      val work = new mutable.ArrayStack[A]()
      work push root
      val visited = mutable.HashSet[A](root)
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
    println("# " + forest.size)
    forest
  }

}
