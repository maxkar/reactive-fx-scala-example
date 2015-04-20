package ru.maxkar.ui.image

import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.AlphaComposite
import java.awt.image.BufferedImage
import javax.swing.JComponent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/** Image fragment UI and renderer. */
private[image] final object FragmentUI {
  /**
   * Renders an image into the component.
   * @param fragment image fragment to display.
   * @param alpha image display alpha.
   */
  def render(
        fragment : Behaviour[ImageFragment], alpha : Behaviour[Double])(
        implicit ctx : BindContext)
      : JComponent = {

    val res = new JComponent() {

      override def paintComponent(g : Graphics) : Unit = {
        super.paintComponent(g)

        val frag = fragment.value
        if (frag == null || frag.base == null)
          return

        val size = this.getSize()
        val dx = (size.width - frag.size.width) / 2
        val dy = (size.height - frag.size.height) / 2

        val g2 = g.asInstanceOf[Graphics2D]
        val oldComp = g2.getComposite
        if (alpha.value < 1.0)
          g2 setComposite AlphaComposite.getInstance(
            AlphaComposite.SRC_OVER, alpha.value.toFloat)

        g.drawImage(
          frag.base,
          dx, dy, dx + frag.size.width, dy + frag.size.height,
          frag.start.x, frag.start.y, frag.size.width, frag.size.height,
          null)

        g2 setComposite oldComp
      }
    }

    alpha ≺ (_ ⇒ res.repaint())
    fragment ≺ (frag ⇒ {
      res.setPreferredSize(if (frag != null) frag.size else new Dimension(1, 1))
      res.revalidate()
      res.repaint()
    })

    res
  }
}
