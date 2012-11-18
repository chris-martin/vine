package vine

import javax.media.opengl.{GLAutoDrawable, GL2, GL}

object OpenGLImplicits {
  implicit def GLToGL2(gl:GL):GL2 = gl.getGL2
  implicit def GLAutoDrawableToGL(x:GLAutoDrawable):GL = x.getGL
  implicit def GLAutoDrawableToGL2(x:GLAutoDrawable):GL2 = x.getGL.getGL2
}
