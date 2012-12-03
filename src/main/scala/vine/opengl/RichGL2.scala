package vine.opengl

import javax.media.opengl.GL2
import vine.geometry.geometry3._
import vine.color.Color

class RichGL2(val gl: GL2) {

  def degrees(a: Float): Float = (a * 180 / math.Pi).toFloat

  def translate(a: Vec) {
    gl.glTranslatef(a.x, a.y, a.z)
  }

  def rotate(_a: Vec) {
    val a = _a.unit
    gl glRotatef( // https://github.com/curran/renderCyliner
      degrees(math.acos(a dot xyz(0,0,1)).toFloat * (if (a.z < 0) -1 else 1)),
      -1 * a.y * a.z, a.x * a.z, 0)
  }

  def draw(p: Vec) {
    gl glVertex3f(p.x, p.y, p.z)
  }

  def clearColor(c: Color) {
    gl glClearColor(c.r, c.g, c.b, c.a)
  }

}
