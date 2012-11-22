package vine.geometry

sealed trait Side {
  def i:Int
}

object LeftSide extends Side {
  override def i = -1
}

object RightSide extends Side {
  override def i = 1
}
