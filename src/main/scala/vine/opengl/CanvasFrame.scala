package vine.opengl

import java.awt, awt.{BorderLayout,Toolkit}

class CanvasFrame (val canvas: awt.Canvas, val frameTitle: String)
    extends javax.swing.JFrame(frameTitle) {

  getContentPane setLayout new BorderLayout
  getContentPane add(canvas, BorderLayout.CENTER)

  val screenSize = Toolkit.getDefaultToolkit.getScreenSize

  setSize(getContentPane getPreferredSize)
  setLocation((screenSize.width - getWidth) / 2, (screenSize.height - getHeight) / 2)
  setVisible(true)

}
