package vine.collection

import collection.mutable
import collection.mutable.ArrayBuffer

class Forest[A] extends Iterable[A] {

  private val all = new mutable.HashSet[A]()

  // child -> parent
  private val childToParent = new mutable.HashMap[A, A]

  // parent -> children
  private val parentToChildren =
    new mutable.HashMap[A, mutable.Set[A]] with mutable.MultiMap[A, A]

  override def iterator = all.iterator

  def add(a: A) {
    all add a
  }

  def add(parent: Option[A], child: A) {
    all add child
    parent foreach { add(_, child) }
  }

  def add(parent: A, child: A) {
    all add parent
    all add child
    childToParent(child) = parent
    parentToChildren addBinding (parent, child)
  }

  def roots: Iterable[A] =
    all filterNot { a => childToParent contains a }

  def ++=(o: Forest[A]) {
    all ++= o.all
    childToParent ++= o.childToParent
    parentToChildren ++= o.parentToChildren
  }

  def childrenOf(parent: A): Seq[A] =
    parentToChildren.get(parent).flatten.toSeq

  def parentOf(child: A): Option[A] =
    childToParent.get(child)

  def layers: Seq[Iterable[A]] = {
    val layers = new ArrayBuffer[ArrayBuffer[A]]()
    def setDepth(a: A, i: Int) {
      while (layers.size < i + 1) {
        layers.append(new ArrayBuffer[A]())
      }
      layers(i).append(a)
    }
    for (root <- roots) {
      val work = new mutable.Stack[(A, Int)]()
      work push ((root, 0))
      while (work.nonEmpty) {
        val (a, depth) = work pop()
        setDepth(a, depth)
        for (child <- childrenOf(a)) {
          work push ((child, depth + 1))
        }
      }
    }
    layers
  }

  def subforestOfDepth(maxDepth: Int): Forest[A] = {
    val subforest = new Forest[A]()
    for (root <- roots) {
      subforest add root
      val work = new mutable.Stack[(A, Int)]()
      work push ((root, 0))
      while (work.nonEmpty) {
        val (a, depth) = work pop()
        if (depth < maxDepth) {
          subforest.add(child = a, parent = parentOf(a))
          for (child <- childrenOf(a)) {
            work push ((child, depth + 1))
          }
        }
      }
    }
    subforest
  }

  def edges: Forest[(A,A)] = {
    val edges = new Forest[(A,A)]()
    for (rootA <- roots; rootB <- childrenOf(rootA)) {
      val work = new mutable.Stack[(A,A)]()
      work push ((rootA, rootB))
      while (work.nonEmpty) {
        val (a, b) = work pop()
        for (c <- childrenOf(b)) {
          work push ((b,c))
          edges add ((a,b), (b,c))
        }
      }
    }
    edges
  }
}

object Forest {

  def join[A](xs: Iterable[Forest[A]]): Forest[A] = {
    val joined = new Forest[A]()
    for (x <- xs) joined ++= x
    joined
  }

}
