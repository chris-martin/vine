package vine.opengl

import javax.media.opengl.GL2
import vine.color.Color
import javax.media.opengl.GL._
import javax.media.opengl.fixedfunc.GLLightingFunc._

trait Material {

  def set(gl: GL2)

}

class DefaultMaterial (val color: Color) extends Material {

  override def set(gl: GL2) {
    import gl._
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, color)
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, color)
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, vine.color.white.toFloatBuffer)
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, vine.color.black.transparent.toFloatBuffer)
    glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 8)
  }

}
