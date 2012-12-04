package vine.collection

import collection.mutable
import collection.mutable.ArrayBuffer
import vine.collection.Forest.EdgeConstructor

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

  def remove(a: A) {
    all -= a
    for (child <- parentToChildren.get(a).flatten) childToParent -= child
    parentToChildren -= a
  }

  def remove(as: Iterable[A]) {
    for (a <- as) remove(a)
  }

  def remove(suchThat: A => Boolean) {
    val as = new ArrayBuffer[A]()
    for (a <- this) if (suchThat(a)) as.append(a)
    remove(as)
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

  def edges[B <: Forest.Edge[A]](implicit makeEdge: EdgeConstructor[A, B]): Forest[B] = {
    val edges = new Forest[B]()
    for (rootA <- roots; rootB <- childrenOf(rootA)) {
      val work = new mutable.Stack[B]()
      work push makeEdge(rootA, rootB)
      while (work.nonEmpty) {
        val ab = work pop()
        val b = ab.edgePoint2
        for (c <- childrenOf(b)) {
          val bc = makeEdge(b, c)
          work push bc
          edges add (ab, bc)
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

  trait Edge[A] {
    def edgePoint1: A
    def edgePoint2: A
  }

  trait EdgeConstructor[A, B] {
    def apply(a1: A, a2: A): B
  }

  class TupleEdgeConstructor[A] extends EdgeConstructor[A, (A, A)] {
    def apply(a1: A, a2: A) = (a1, a2)
  }

}
