package vine.opengl

import vine.geometry.geometry3._
import javax.media.opengl.GL2
import javax.media.opengl.fixedfunc.GLMatrixFunc._
import javax.media.opengl.glu.GLU
import java.awt.Dimension

class Camera(var up: Vec, var view: Line, glu: GLU, dim: Dimension) {

  val widthHeightRatio = dim.width / dim.height

  import glu._

  def set(gl: GL2) {

    import gl._

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()

    gluPerspective(10, widthHeightRatio, 1, 1000)
    gluLookAt(
      view.a.x, view.a.y, view.a.z,
      view.b.x, view.b.y, view.b.z,
      up.x, up.y, up.z)

  }

}
