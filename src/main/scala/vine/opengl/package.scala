package vine

import javax.media.opengl.{GLAutoDrawable, GL2, GL, GLProfile, GLCapabilities}

package object opengl {

  val glProfile = GLProfile.getDefault

  object capabilities extends GLCapabilities(glProfile) {
    setRedBits(8)
    setBlueBits(8)
    setGreenBits(8)
    setAlphaBits(8)
  }

  implicit def enrichGL2(gl: GL2) = new RichGL2(gl)

  implicit def GLToGL2(gl:GL):GL2 = gl.getGL2
  implicit def GLAutoDrawableToGL(x:GLAutoDrawable):GL = x.getGL
  implicit def GLAutoDrawableToGL2(x:GLAutoDrawable):GL2 = x.getGL.getGL2

}
