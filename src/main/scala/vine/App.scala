package vine

import javax.media.opengl.glu.GLU
import com.jogamp.opengl.util.FPSAnimator
import java.awt.event._
import vine.geometry._
import javax.media.opengl._
import fixedfunc.GLMatrixFunc._
import javax.media.opengl.awt.GLCanvas
import javax.swing.JFrame
import java.awt.{Toolkit, BorderLayout}
import javax.media.opengl.GL._
import javax.media.opengl.fixedfunc.GLLightingFunc._
import javax.media.opengl.GL2._
import scala.Some
import vine.OpenGLImplicits._
import vine.color.implicits._

class App {

  val glu = new GLU
  import glu._

  val mesh = Ply.parse(Ply.getClass.getResourceAsStream("bun_zipper_res3.ply"))
  mesh.center()
  println(mesh)

  val animator = new FPSAnimator(canvas, 20)

  object keyListener extends KeyAdapter {
    override def keyReleased(e: KeyEvent) {
      val step = camera.view.ab.mag(3)
      e.getKeyCode match {
        case KeyEvent.VK_ESCAPE => stop()
        case KeyEvent.VK_S => camera.view -= step
        case KeyEvent.VK_W => camera.view += step
        case _ =>
      }
    }
  }

  object mouseListener extends MouseAdapter {

    var previous:Option[Vec2] = None

    override def mousePressed(e: MouseEvent) {
      previous = Some(xy(e.getX, e.getY))
    }

    override def mouseReleased(e: MouseEvent) {
      previous = None
    }

    override def mouseDragged(e: MouseEvent) {
      val b = xy(e.getX, e.getY)
      previous match { case Some(a) => mouseDragged(aToB(a, b)); case None => }
      previous = Some(b)
    }

    def mouseDragged(drag:Line2) {
      val eye = camera.view.a
      val target = camera.view.b
      val forward = camera.view.ab.unit
      val right = camera.up.cross(forward).unit
      val up = right.cross(forward).unit
      var newEye = eye
      newEye = rotatePointAroundLine(pointAndStep(target, up), newEye, drag.ab.x/100)
      newEye = rotatePointAroundLine(pointAndStep(target, right), newEye, drag.ab.y/100)
      camera.view = camera.view.a(newEye)
    }

  }

  frame.addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent) { stop() }
  })

  animator.start()

  def stop() {
    animator.stop()
    frame.dispose()
  }

  object capabilities extends GLCapabilities(GLProfileSingleton.glProfile) {
    setRedBits(8); setBlueBits(8); setGreenBits(8); setAlphaBits(8)
  }

  object canvas extends GLCanvas(capabilities) {
    private val WIDTH = 1000
    private val HEIGHT = 800
    setSize(WIDTH, HEIGHT)
    setIgnoreRepaint(true)
    addGLEventListener(renderer)
    addMouseListener(mouseListener)
    addMouseMotionListener(mouseListener)
    addKeyListener(keyListener)
  }

  object frame extends JFrame("Vine") {
    getContentPane setLayout new BorderLayout
    getContentPane add(canvas, BorderLayout.CENTER)

    val screenSize = Toolkit.getDefaultToolkit.getScreenSize

    setSize(getContentPane getPreferredSize)
    setLocation((screenSize.width - getWidth) / 2, (screenSize.height - getHeight) / 2)
    setVisible(true)
  }

  val faceColor = color("#e73")
  val wireColor = color("#3332")

  object renderer extends GLEventListener {

    def display(glDrawable: GLAutoDrawable) {
      val gl:GL2 = (glDrawable getGL).getGL2
      camera set glDrawable
      gl glClear GL_COLOR_BUFFER_BIT
      faces(gl)
      frame(gl)
      gl glFlush()
    }

    def faces(gl:GL2) {
      import gl._

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, faceColor)
      glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, faceColor)
      glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 4)

      glBegin(GL_TRIANGLES)
      for (t:mesh.Triangle <- mesh.getTriangles) {
        for (p:Vec3 <- t.vertexLocations) {
          glVertex3f(p.x, p.y, p.z)
        }
      }
      glEnd()

    }

    def frame(gl:GL2) {
      import gl._

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, wireColor)
      glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, wireColor)
      glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 0)

      glBegin(GL_LINES)
      for (e:mesh.Edge <- mesh.getEdges) for (v <- e.locations) draw(gl, v)
      glEnd()

    }

    def draw(gl: GL2, v: Vec3) { gl glVertex3f(v.x, v.y, v.z) }

    def dispose(p1: GLAutoDrawable) { }

    def init(glDrawable: GLAutoDrawable) {
      val gl = (glDrawable getGL).getGL2
      import gl._

      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glEnable( GL_LINE_SMOOTH )
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
      glClearColor(0.5f, 0.65f, 0.8f, 0)

      glEnable(GL_LIGHTING)
      glEnable(GL_LIGHT0)
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE)
      glLightfv(GL_LIGHT0, GL_AMBIENT, Array(0.2f, 0.2f, 0.2f, 1f), 0)
      glLightfv(GL_LIGHT0, GL_DIFFUSE, Array(1f, 1f, 1f, 1f), 0)
      glLightfv(GL_LIGHT0, GL_POSITION, Array(10f, 10, 10, 1), 0)

    }

    def reshape(glDrawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int) { }

  }

  object camera {

    var up:Vec3 = xyz(0, 1, 0)

    var view:Line3 = aToB(xyz(0, 0, 2), origin3)

    def set(gl:GL2) {
      gl glMatrixMode(GL_PROJECTION)
      gl glLoadIdentity()

      val widthHeightRatio = canvas.getWidth / canvas.getHeight
      gluPerspective(10, widthHeightRatio, 1, 1000)
      gluLookAt(
        view.a.x, view.a.y, view.a.z,
        view.b.x, view.b.y, view.b.z,
        up.x, up.y, up.z)
    }

  }

}