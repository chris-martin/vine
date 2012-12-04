package vine

import vine.collection.Forest
import geometry.geometry3._
import mesh.Ply

import scala.collection.immutable
import javax.media.opengl.glu.{GLU, GLUquadric}

class App {

  val mesh = Ply.parse(Ply.getClass.getResourceAsStream("bun_zipper_res2.ply"))
  import mesh.{Vertex, Edge, Component, Corner, LR, Triangle, triangles, vertexToLocation, cornerToLocation}

  mesh.translate({
    val bb = mesh.boundingBox
    midpoint(bb._1, bb._2).y(bb._1.y + 0.007f) * -1
  })
  println(mesh)

  var mark: Option[Corner] = None

  val lrs: Seq[LR] = mesh.lr(
    firstCorner = (component: Component) =>
      component.flatMap(_.corners).find(_.vertex.location.y < 0).get
  )
  val lrTriangles = immutable.HashSet[Triangle](lrs.flatMap(lr => lr.triangles):_*)

  val vineTriangleTree: Forest[Triangle] = Forest.join(
    lrs.map(_.triangles.toTree(_.corners.head.vertex.location.y < 0))
  )

  val subtreeDepths = vineTriangleTree.subtreeDepths

  case class TrianglePair (_1: Triangle, _2: Triangle) extends Forest.Edge[Triangle] {
    override def edgePoint1 = _1
    override def edgePoint2 = _2

    private var _joinPoint: Option[Vec] = None
    def joinPoint: Vec = _joinPoint match {
      case Some(x) => x
      case None => _joinPoint = Some({
        val e = (_1 sharedEdges _2).head
        midpoint(e.vertices(0), e.vertices(1))
      }); _joinPoint.get
    }

    def isUnderground: Boolean = joinPoint.y < 0
  }

  object TrianglePairConstructor extends Forest.EdgeConstructor[Triangle, TrianglePair] {
    override def apply(a1: Triangle, a2: Triangle): TrianglePair = new TrianglePair(a1, a2)
  }

  val vineTrianglePairTree: Forest[TrianglePair] = vineTriangleTree.edges(TrianglePairConstructor)

  case class TriangleTrio (_1: Triangle, _2: Triangle, _3: Triangle) extends Forest.Edge[TrianglePair] {
    override def edgePoint1 = TrianglePair(_1, _2)
    override def edgePoint2 = TrianglePair(_2, _3)
    def isUnderground: Boolean = edgePoint1.isUnderground && edgePoint2.isUnderground
  }

  object TriangleTrioConstructor extends Forest.EdgeConstructor[TrianglePair, TriangleTrio] {
    override def apply(a1: TrianglePair, a2: TrianglePair): TriangleTrio =
      new TriangleTrio(a1.edgePoint1, a1.edgePoint2, a2.edgePoint2)
  }

  val vineTriangleTrioTree: Forest[TriangleTrio] = vineTrianglePairTree.edges(TriangleTrioConstructor)

  vineTriangleTrioTree.remove((trio: TriangleTrio) => trio.isUnderground)

  val vineTriangleDepths: Seq[Iterable[TriangleTrio]] = vineTriangleTrioTree.layers
  val vineVertices: Seq[Seq[Vertex]] = (
    lrs
      .flatMap(_.cycle.split(_.location.y < 0))
      .flatMap(seq => List(
        seq.slice(0, seq.size / 2),
        seq.reverse.slice(0, (seq.size + 1) / 2)
      ))
  )

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

    val cycleFace = List(
      new DefaultMaterial(color.parse("#e73")),
      new DefaultMaterial(color.parse("#fdc"))
    )
    val face = new DefaultMaterial(color.parse("#bbb"))
    val wire = new DefaultMaterial(color.parse("#000c"))
    val floor = new DefaultMaterial(color.parse("#ce8632"))
  }

  object renderer extends javax.media.opengl.GLEventListener {

    import vine.geometry.geometry3._
    import opengl._
    import javax.media.opengl.{GL2,GLAutoDrawable,fixedfunc}
    import javax.media.opengl.GL._, javax.media.opengl.GL2._
    import fixedfunc.GLLightingFunc._, fixedfunc.GLMatrixFunc._

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

      def drawEdge(a: Vec, b: Vec, thickness: Float = 0.0002f, extraLength: Float = 0) {
        gl glPushMatrix()
        gl translate(a subY extraLength/2)
        gl rotate(b - a)
        glu.gluCylinder(gluQuad, thickness, thickness, distance(a, b) + extraLength, 3, 1)
        gl glPopMatrix()
      }

    }
    implicit def richGL(gl: GL2) = new RichGL(gl)

    var isAnimating = true

    def display(glDrawable: GLAutoDrawable) {

      val gl:GL2 = (glDrawable getGL).getGL2
      import gl._
      camera set glDrawable
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()
      drawFloor(gl)
      drawFaces(gl)
      //drawCycleFaces(gl)
      //drawFrame(gl)
      //drawCycle(gl)
      //drawMark(gl)
      //drawLrCycleVine(gl)
      drawLrTriangleVine(gl)
      glFlush()
      if (isAnimating) vineAnimationStep += 1
    }

    def drawLrCycleVine(gl: GL2) {
      gl setMaterial new DefaultMaterial(color.parse("#092"))
      for (seq <- vineVertices) {
        val slice = seq.slice(0, (vineAnimationStep * 1.2f).toInt)
        for ((a, b) <- slice.zip(slice.drop(1))) {
          gl drawEdge(a, b, 0.0012f)
        }
      }
    }

    def drawLrTriangleVine(gl: GL2) {
      gl setMaterial new DefaultMaterial(color.parse("#092"))
      val maxDepth = (vineAnimationStep * 2f).toInt
      if (isAnimating && maxDepth > vineTriangleDepths.size) isAnimating = false
      for (depth <- 0 until math.min(maxDepth, vineTriangleDepths.size)) {
        for (trio: TriangleTrio <- vineTriangleDepths(depth)) {
          var thickness = 0.0008f
          thickness += 0.0003f * FastAtan(subtreeDepths(trio._3) - 10).toFloat
          thickness += 0.0003f * FastAtan(maxDepth - depth).toFloat
          thickness *= maxDepth / vineTriangleDepths.size.toFloat
          gl.drawEdge(trio.edgePoint1.joinPoint, trio.edgePoint2.joinPoint,
            thickness = thickness, extraLength = 0.0008f)
        }
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
      gl glBegin(GL_TRIANGLES)
      gl setMaterial material.face
      for (t <- triangles) gl draw t
      gl glEnd()
    }

    def drawCycleFaces(gl:GL2) {
      import gl._
      glBegin(GL_TRIANGLES)
      gl setMaterial material.cycleFace(0)
      for (t <- triangles) if (lrTriangles contains t) gl draw t
      gl setMaterial material.cycleFace(1)
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
      gl clearColor(color.parse("#68c"))

      glEnable(GL_LIGHTING)
      glEnable(GL_LIGHT0)
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE)
      glLightfv(GL_LIGHT0, GL_AMBIENT, Array(0, 0, 0, 1f), 0)
      glLightfv(GL_LIGHT0, GL_DIFFUSE, Array(1, 1, 1, 1f), 0)
      glLightfv(GL_LIGHT0, GL_SPECULAR, Array(1f, 1f, 1f, 1f), 0)
      glLightfv(GL_LIGHT0, GL_POSITION, Array(1, 2, 2, 0f), 0)
    }

    def reshape(glDrawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int) { }

  }

}

object FastAtan {
  val values: Seq[Float] = for (i <- 0 until 5000) yield { math.atan(i / 1000).toFloat }
  def apply(x: Float): Float = (if (x < 0) -1 else 1) *
    (if (math.abs(x) > 5) 1 else values((math.abs(x).toInt / 1000)))
}

object Main {
  def main(args: Array[String]) {
    new App()
  }
}
