package vine.collection

import collection.mutable

class RichMap[A, B] (val map: Map[A, B]) {

  // http://stackoverflow.com/questions/3678970/what-is-the-most-succinct-scala-way-to-reverse-a-map
  def invert: mutable.MultiMap[B, A] =
    (new mutable.HashMap[B, mutable.Set[A]] with mutable.MultiMap[B,A]) ++=
      map.groupBy(_._2).mapValues(mutable.Set[A]() ++= _.keys)

}
