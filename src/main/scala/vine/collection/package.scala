package vine

import scala.collection.mutable

package object collection {

  implicit def enrichMap[A, B](map: Map[A, B]): RichMap[A, B] = new RichMap(map)

  def mergeMultimaps[A, B](maps: Map[A, Set[B]]*): mutable.MultiMap[A, B] = {
    null
  }

}
