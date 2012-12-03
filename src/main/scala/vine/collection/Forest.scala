package vine.collection

import collection.mutable

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

}

object Forest {

  def join[A](xs: Iterable[Forest[A]]): Forest[A] = {
    val joined = new Forest[A]()
    for (x <- xs) joined ++= x
    joined
  }

}
