package vine

class PairIterable[A] (val aIterable: Iterable[A]) extends Iterable[List[A]] {

  def iterator = new PairIterator

  class PairIterator extends Iterator[List[A]] {

    var aIterator1 = aIterable.iterator
    var aIterator2 = aIterable.iterator

    var a1 = null
    var a2 = null

    def next() : List[A] = {
    }

  }

}
