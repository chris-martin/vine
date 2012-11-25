package vine.collection

import collection.mutable

class CircularSet[A] extends Iterable[A] {

  // a -> (a.prev, a.next)
  private val entries = new mutable.HashMap[A, Pair[A,A]]

  sealed trait Direction
  object Forward extends Direction
  object Backward extends Direction

  private def link(prev: Option[A], curr: A, next: Option[A]) {
    entries.put(curr, (
      prev getOrElse { entries.get(curr).get._1 },
      next getOrElse { entries.get(curr).get._2 }
    ))
  }

  def direction(a: A, b: A): Option[Direction] = {
    entries.get(a) match {
      case Some((prev, next)) =>
        if (next == b) Some(Forward)
        else if (prev == b) Some(Backward)
        else None
      case None => None
    }
  }

  def insert(args: A*) {

    println("%d %s".format(size, args))

    if (isEmpty) {
      for (i <- 0 until args.size) {
        link(
          Some(args( (i + args.size - 1) % args.size )),
          args(i),
          Some(args( (i + 1) % args.size ))
        )
      }
      return
    }

    args.size match {
      case 0 => true
      case 1 => entries.contains(args.head)
      case _ => direction(args.head, args.reverse.head) match {
        case Some(d) =>
          val forwardArgs = d match {
            case Forward => args
            case Backward => args.reverse
          }
          link(None, forwardArgs(0), Some(forwardArgs(1)))
          link(Some(forwardArgs(forwardArgs.size - 2)), forwardArgs(forwardArgs.size - 1), None)
          for (i <- 1 until forwardArgs.size - 1) {
            link(
              Some(forwardArgs(i - 1)),
              forwardArgs(i),
              Some(forwardArgs(i + 1))
            )
          }
        case None => //throw new IllegalArgumentException
      }
    }
  }

  override def size: Int = entries.size

  override def isEmpty: Boolean = size == 0

  override def iterator: Iterator[A] =
    if (isEmpty) Iterator.empty
    else iterator(entries.head._1)

  def iterator(start: A): Iterator[A] = new Iterator[A] {

    private var _i = entries.size

    private var _next: Option[A] = Some(start)

    override def hasNext = _next.isDefined

    override def next() = {
      val x = _next.get
      _i -= 1
      _next =
        if (_i == 0) None
        else entries.get(x).map(y => y._2)
      x
    }

  }

  override def toString = "CircularSet of size %d".format(size)

}
