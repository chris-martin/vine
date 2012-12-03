package vine

import geometry.geometry3._
import mesh.Ply

import scala.collection.immutable
import javax.media.opengl.glu.{GLU, GLUquadric}
import vine.collection.{enrichMap, mergeMultiMaps}

class App {

  val mesh = Ply.parse(Ply.getClass.getResourceAsStream("bun_zipper_res3.ply"))
  mesh.translate({
    val bb = mesh.boundingBox
    midpoint(bb._1, bb._2).y(bb._1.y + 0.007f) * -1
  })
  println(mesh)

  var mark: Option[mesh.Corner] = None

  val lrs: Seq[mesh.LR] = mesh.lr
  val lrTriangles = immutable.HashSet[mesh.Triangle](lrs.flatMap(lr => lr.triangles):_*)
  val vineVertices: Seq[Seq[mesh.Vertex]] = lrs.
    flatMap(_.cycle.split(v => v.location.y < 0)).
    flatMap(seq => List(seq.slice(0, seq.size / 2), seq.reverse.slice(0, (seq.size + 1) / 2)))

  val glu = new GLU

  val fps = 20
  val animator = new com.jogamp.opengl.util.FPSAnimator(canvas, fps)

  val camera = new opengl.Camera(
    up = xyz(0, 1, 0),
    view = aToB(
      xyz(-0.5, 0.5, 2),
      xyz(0, 0.07, 0)
    ),
    glu = glu,
    dim = canvas.getSize
  )

  val frame = new opengl.CanvasFrame(canvas, "Vine")

  object keyListener extends java.awt.event.KeyAdapter {

    import java.awt.event.KeyEvent, KeyEvent._

    override def keyReleased(e: KeyEvent) {
      e.getKeyCode match {
        case VK_ESCAPE => stop()
        case VK_M => mark = mark match {
          case Some(x) => None
          case None => mesh.triangles.headOption flatMap { t => t.corners.headOption }
        }
        case VK_N => mark = mark map { m => m.next }
        case VK_P => mark = mark map { m => m.prev }
        case VK_S => mark = mark map { m => m.swing getOrElse m }
        case VK_V => renderer.vineAnimationStep = 0
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
    override def windowClosing(e: java.awt.event.WindowEvent) { stop() }
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
    val floor = new DefaultMaterial(color.parse("#5e2612"))
  }

  object renderer extends javax.media.opengl.GLEventListener {

    import vine.geometry.geometry3._
    import opengl._
    import javax.media.opengl.{GL2,GLAutoDrawable,fixedfunc}
    import javax.media.opengl.GL._, javax.media.opengl.GL2._
    import fixedfunc.GLLightingFunc._, fixedfunc.GLMatrixFunc._
    import mesh.{Vertex, Edge, Triangle, triangles, vertexToLocation, cornerToLocation}

    var gluQuad: GLUquadric = null

    var vineAnimationStep: Int = 0

    class RichGL (val gl: GL2) {

      def setMaterial(m: Material) {
        m set gl
      }

      def draw(t: Triangle) {
        for (v <- t.vertices) draw(v)
      }

      def draw(v: Vertex) {
        gl draw(v.location)
      }

      def draw(e: Edge) {
        drawEdge(e.locations(0), e.locations(1))
      }

      def drawEdge(a: Vec, b: Vec, thickness: Float = 0.0002f) {
        gl glPushMatrix()
        gl translate(a)
        gl rotate(b-a)
        glu.gluCylinder(gluQuad, thickness, thickness, distance(a, b), 3, 3)
        gl glPopMatrix()
      }

    }
    implicit def richGL(gl: GL2) = new RichGL(gl)

    def display(glDrawable: GLAutoDrawable) {

      val gl:GL2 = (glDrawable getGL).getGL2
      import gl._
      camera set glDrawable
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()
      drawFloor(gl)
      //drawFaces(gl)
      drawFrame(gl)
      drawCycle(gl)
      //drawMark(gl)
      drawVine(gl)
      glFlush()
    }

    def drawVine(gl: GL2) {
      gl setMaterial new DefaultMaterial(color.red)
      vineAnimationStep += 1
      for (v <- vineVertices.flatMap(seq => seq.slice(0, (vineAnimationStep * 1f).toInt))) {
        gl glPushMatrix()
        gl translate v.location
        glu.gluSphere(gluQuad, 0.002f, 4, 4)
        gl glPopMatrix()
      }
    }

    def drawFloor(gl: GL2) {
      gl setMaterial material.floor
      gl glPushMatrix()
      gl glRotatef (90, 1, 0, 0)
      glu gluDisk (gluQuad, 0, 0.12f, 50, 5)
      gl glPopMatrix()
    }

    def drawMark(gl: GL2) {
      mark.foreach { m =>
        import gl._
        gl setMaterial new DefaultMaterial(color.red)

        glPushMatrix()
        gl translate m
        glu.gluSphere(gluQuad, 0.002f, 4, 4)
        glPopMatrix()

        glBegin(GL_TRIANGLES)
        gl draw m.triangle
        glEnd()

      }
    }

    def drawFaces(gl:GL2) {
      import gl._
      glBegin(GL_TRIANGLES)
      gl setMaterial material.face(0)
      for (t <- triangles) if (lrTriangles contains t) gl draw t
      gl setMaterial material.face(1)
      for (t <- triangles) if (!(lrTriangles contains t)) gl draw t
      glEnd()
    }

    def drawFrame(gl:GL2) {
      gl setMaterial material.wire
      for (e <- mesh.edges) gl draw e
    }

    def drawCycle(gl:GL2) {
      gl setMaterial material.wire
      for (lrc <- lrs; (a,b) <- lrc.cycle.zip(lrc.cycle.slice(1, lrc.cycle.size))) {
        gl drawEdge(a, b, 0.0007f)
      }
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
