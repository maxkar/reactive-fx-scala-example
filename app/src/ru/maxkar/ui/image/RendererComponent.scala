package ru.maxkar.ui.image

import java.awt.Dimension
import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JComponent


/** Simple image rendering component. */
private[image] final class RendererComponent extends JComponent {
  /** Current image to display. */
  private var img : BufferedImage = null

  /** Renderable dimension. */
  private var renderDims : Dimension = null



  /**
   * Updates content of this component.
   * @param display image and renderable size used for display.
   * @param prefSize preferred component size.
   */
  private[image] def updateContent(
        display : (BufferedImage, Dimension),
        prefSize : Dimension)
      : Unit = {
    setPreferredSize(prefSize)
    img = display._1
    renderDims = display._2
    revalidate()
    repaint()
  }



  override def paintComponent(g : Graphics) : Unit = {
    super.paintComponent(g)

    if (img == null)
      return

    val size = this.getSize()

    val dx = (size.width - renderDims.width) / 2
    val dy = (size.height - renderDims.height) / 2


    g.drawImage(
      img,
      dx, dy, dx + renderDims.width, dy + renderDims.height,
      0, 0, renderDims.width, renderDims.height,
      null)
  }
}
