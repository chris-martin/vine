package vine.geometry.geometry3

trait Sphere {

  def center: Vec
  def radius: Float

}

private[geometry3] class SimpleSphere
    ( val _center: Vec, val _radius: Float) extends Sphere {

  override def center = _center
  override def radius = _radius
}
