package vine

import java.awt.event._
import javax.media.opengl._
import awt.GLCanvas
import fixedfunc.GLMatrixFunc
import javax.media.opengl.glu.GLU

import java.awt.{Stroke, Toolkit, BorderLayout}
import javax.swing.JFrame
import com.jogamp.opengl.util.FPSAnimator

import OpenGLImplicits._
import vine.Geometry.Vec3

object Main {
  def main(args: Array[String]) {
    new VineApp()
  }
}

object GLSingleton {
  val glProfile = GLProfile.getDefault
}

class VineApp {

  GLSingleton

  val mesh = Ply.parse(Ply.getClass.getResourceAsStream("bun_zipper_res2.ply"))
  val canvas = new VineCanvas(mesh)
  val frame = new VineFrame(canvas)
  val animator = new FPSAnimator(canvas, 20)

  /*canvas.addKeyListener(new KeyAdapter {
    override def keyReleased(e: KeyEvent) {
      e.getKeyCode match { case _ => stop() }
    }
  })*/

  frame.addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent) { stop() }
  })

  animator.start()

  def stop() {
    animator.stop()
    frame.dispose()
  }

}

class VineCapabilities extends GLCapabilities(GLSingleton.glProfile) {
  setRedBits(8); setBlueBits(8); setGreenBits(8); setAlphaBits(8)
}

class VineCanvas (mesh:Mesh) extends GLCanvas(new VineCapabilities()) {
  private val WIDTH = 800
  private val HEIGHT = 600
  setSize(WIDTH, HEIGHT)
  setIgnoreRepaint(true)
  addGLEventListener(new VineRenderer(this, mesh))
}

class VineFrame(canvas:VineCanvas) extends JFrame("Vine") {
  getContentPane setLayout new BorderLayout
  getContentPane add(canvas, BorderLayout.CENTER)

  val screenSize = Toolkit.getDefaultToolkit.getScreenSize

  setSize(getContentPane getPreferredSize)
  setLocation((screenSize.width - getWidth) / 2, (screenSize.height - getHeight) / 2)
  setVisible(true)
}

class VineRenderer(canvas:VineCanvas, mesh:Mesh) extends GLEventListener {

  val glu = new GLU

  def display(glDrawable: GLAutoDrawable) {
    val gl:GL3 = (glDrawable getGL).getGL3
    gl glClear GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT

    gl.glMatrixMode(GLMatrixFunc.GL_MODELVIEW)
    gl glLoadIdentity()

    gl.glBegin(GL.GL_TRIANGLES)
    gl.glColor3f(0.9f, 0.5f, 0.2f)
    for (t:mesh.Triangle <- mesh.getTriangles) {
      for (p:Vec3 <- t.vertexLocations) {
        gl.glVertex3f(p.x, p.y, p.z)
      }
    }
    gl.glEnd()

    gl.glBegin(GL.GL_LINES)
    gl.glColor3f(1,1,1)
    for (t:mesh.Triangle <- mesh.getTriangles) {
      for (p:Vec3 <- t.vertexLocations) {
        gl.glVertex3f(p.x, p.y, p.z)
      }
    }
    gl.glEnd()

    gl glFlush()
  }

  def dispose(p1: GLAutoDrawable) { }

  def init(glDrawable: GLAutoDrawable) {
    setCamera(glDrawable, 1)
    val gl = (glDrawable getGL).getGL2
    gl glClearColor(0, 0, 0, 0)
  }

  def reshape(glDrawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int) { }

  def setCamera(gl:GL2, distance:Float) {
    gl.glMatrixMode(GLMatrixFunc.GL_PROJECTION)
    gl.glLoadIdentity()
    val widthHeightRatio = canvas.getWidth / canvas.getHeight
    glu.gluPerspective(45, widthHeightRatio, 1, 1000)
    glu.gluLookAt(0, 0, distance, 0, 0, -10, 0, 1, 0)
  }

}
