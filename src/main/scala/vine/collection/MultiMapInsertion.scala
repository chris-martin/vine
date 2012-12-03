package vine.collection

import collection.mutable.{HashSet, Set, Map}

trait MultiMapInsertion[A, B] extends Map[A, Set[B]] {

  protected def makeSet: Set[B] = new HashSet[B]

  def addBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        val set = makeSet
        set += value
        this(key) = set
      case Some(set) =>
        set += value
    }
    this
  }

  def ensureContains(key: A): this.type = {
    if (!contains(key)) {
      this(key) = makeSet
    }
    this
  }

}
