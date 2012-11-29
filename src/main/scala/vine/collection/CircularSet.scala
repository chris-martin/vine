package vine.collection

import collection.{immutable, mutable}

class CircularSet[A] extends Iterable[A] {

  // a -> (a.prev, a.next)
  private val entries = new mutable.HashMap[A, Pair[A,A]]

  sealed trait Direction {
    def pairEntry(pair: (A, A)): A
  }
  object Backward extends Direction {
    override def pairEntry(pair: (A, A)) = pair._1
  }
  object Forward extends Direction {
    override def pairEntry(pair: (A, A)) = pair._2
  }

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
        case None =>
      }
    }
  }

  override def size: Int = entries.size

  override def isEmpty: Boolean = size == 0

  def distances(zero: A => Boolean): Map[A, Int] = {
    val distances = mutable.HashMap[A, Int]()
    for (
      start <- find(zero);
      direction <- List(Forward, Backward)
    ) {
      var distanceFromPreviousZero = 0
      for (element <- iterator(start, direction)) {
        if (zero(element)) {
          distanceFromPreviousZero = 0
        }
        distances.put(element, distances.get(element) match {
          case Some(x) => math.min(x, distanceFromPreviousZero)
          case None => distanceFromPreviousZero
        })
        distanceFromPreviousZero += 1
      }
    }
    distances.toMap
  }

  private def arbitraryElement: Option[A] =
    if (isEmpty) None else Some(entries.head._1)

  override def iterator = iterator(Forward)

  def iterator(direction: Direction): Iterator[A] =
    arbitraryElement match {
      case Some(a) => iterator(a, direction)
      case None => Iterator.empty
    }

  def iterator(start: A, direction: Direction = Forward): Iterator[A] =
    new Iterator[A] {
      private var _i = entries.size
      private var _next: Option[A] = Some(start)
      override def hasNext = _next.isDefined
      override def next() = {
        val x = _next.get
        _i -= 1
        _next =
          if (_i == 0) None
          else entries.get(x).map(direction.pairEntry)
        x
      }
    }

  override def toString() = "CircularSet of size %d".format(size)

}
