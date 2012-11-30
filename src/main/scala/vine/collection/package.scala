package vine

import scala.collection.mutable

package object collection {

  implicit def enrichMap[A, B](map: Map[A, B]): RichMap[A, B] = new RichMap(map)

  def mergeMultiMaps[A, B](
    maps: Seq[scala.collection.Map[A, Iterable[B]]]
  ): mutable.MultiMap[A, B] = {
    val merged = new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A,B]
    for (map <- maps; entry <- map) {
      val key = entry._1
      for (value <- entry._2) {
        merged.addBinding(key, value)
      }
    }
    merged
  }

}
