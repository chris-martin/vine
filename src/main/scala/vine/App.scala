package vine

import geometry.geometry3._
import mesh.Ply

import scala.collection.immutable
import javax.media.opengl.glu.{GLU,GLUquadric}

class App {

  val mesh = Ply.parse(Ply.getClass.getResourceAsStream("bun_zipper_res3.ply"))
  mesh.translateCenterToOrigin()
  println(mesh)

  val lr = mesh.lr
  val lrt = immutable.HashSet[mesh.Triangle](lr.flatMap(lr => lr.triangles):_*)
  val lre = lrt.flatMap(t => t.undirectedEdges)

  val glu = new GLU
  val animator = new com.jogamp.opengl.util.FPSAnimator(canvas, 20)
  val camera = new opengl.Camera(xyz(0, 1, 0), aToB(xyz(0, 0, 2), origin), glu, canvas.getSize)
  val frame = new opengl.CanvasFrame(canvas, "Vine")

  object keyListener extends java.awt.event.KeyAdapter {

    import java.awt.event.KeyEvent

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

  object mouseListener extends java.awt.event.MouseAdapter {

    import vine.geometry._, geometry2._, geometry3.{rotatePointAroundLine, pointAndStep}
    import java.awt.event.MouseEvent

    var previous:Option[Vec] = None

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

    def mouseDragged(drag:Line) {
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

  frame.addWindowListener(new java.awt.event.WindowAdapter {

    import java.awt.event.WindowEvent

    override def windowClosing(e: WindowEvent) { stop() }
  })

  animator.start()

  def stop() {
    animator.stop()
    frame.dispose()
  }

  object canvas extends opengl.Canvas {
    setSize(1000, 800)
    addGLEventListener(renderer)
    addMouseListener(mouseListener)
    addMouseMotionListener(mouseListener)
    addKeyListener(keyListener)
  }

  object material {

    import vine.color
    import opengl.DefaultMaterial

    val face = List(
      new DefaultMaterial(color.parse("#e73")),
      new DefaultMaterial(color.parse("#fdc"))
    )
    val wire = new DefaultMaterial(color.parse("#000c"))
  }

  object renderer extends javax.media.opengl.GLEventListener {

    import vine.geometry.geometry3._
    import opengl._
    import javax.media.opengl.{fixedfunc,GL2,GLAutoDrawable}
    import javax.media.opengl.GL._, javax.media.opengl.GL2._
    import fixedfunc.GLLightingFunc._, fixedfunc.GLMatrixFunc._
    import mesh.{Vertex, Edge, Triangle, triangles, vertexToLocation}

    var gluQuad: GLUquadric = null

    class RichGL (val gl: GL2) {

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

      def setMaterial(m: Material) {
        m set gl
      }

      def draw(t: Triangle) {
        for (v <- t.vertices) draw(v)
      }
      def draw(v: Vertex) {
        draw(v.location)
      }
      def draw(p: Vec) {
        gl glVertex3f(p.x, p.y, p.z)
      }

      def draw(e: Edge) {
        drawEdge(e.locations(0), e.locations(1))
      }
      def drawEdge(a: Vec, b: Vec) {
        gl glPushMatrix()
        translate(a)
        rotate(b-a)
        glu.gluCylinder(gluQuad, 0.0002, 0.0002, distance(a, b), 3, 3)
        gl glPopMatrix()
      }

    }
    implicit def richGL(gl: GL2) = new RichGL(gl)

    def display(glDrawable: GLAutoDrawable) {

      val gl:GL2 = (glDrawable getGL).getGL2
      camera set glDrawable
      gl glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      faces(gl)
      frame(gl)
      gl glFlush()
    }

    def faces(gl:GL2) {

      import gl._

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      glBegin(GL_TRIANGLES)
      gl setMaterial material.face(0)
      for (t <- triangles) if (lrt contains t) gl draw t
      gl setMaterial material.face(1)
      for (t <- triangles) if (!(lrt contains t)) gl draw t
      glEnd()
    }

    def frame(gl:GL2) {

      import gl._

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      gl setMaterial material.wire
      for (lrc <- lr) {
        for ((a,b) <- lrc.cycle.zip(lrc.cycle.slice(1, lrc.cycle.size))) {
          gl drawEdge(a, b)
        }
      }
      //for (e <- mesh.edges) if (lre contains e) gl draw e
    }

    def dispose(p1: GLAutoDrawable) { }

    def init(glDrawable: GLAutoDrawable) {

      val gl = glDrawable getGL() getGL2()
      import gl._

      gluQuad = glu.gluNewQuadric()

      glEnable(GL_BLEND)
      glEnable(GL_DEPTH_TEST)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glEnable(GL_LINE_SMOOTH)
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

}

object Main {
  def main(args: Array[String]) {
    new App()
  }
}
