package ru.maxkar.ui.image

import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.event.ComponentListener
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent

import javax.swing.JComponent
import javax.swing.JScrollPane
import javax.swing.JPanel
import javax.swing.ScrollPaneConstants

import ru.maxkar.ui.Scrolls
import ru.maxkar.ui.Zoom

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/**
 * Pane used to display one (and only one) image.
 * @param image image to display.
 * @param zoom zoom model.
 * @param lifespan image/binding lifespan.
 */
final class ImagePane(
      val ui : JComponent,
      val effectiveZoom : Behaviour[Double])



/**
 * Image object companion.
 */
object ImagePane {
  /**
   * Renders an image (creates an image display
   * component).
   * @param image image to display.
   * @param zoom current (desizer) zoom.
   * @param ctx value binding context.
   * @return image behaviour.
   */
  def render(
        image : BufferedImage,
        zoom : Behaviour[Zoom])(
        implicit ctx : BindContext)
      : ImagePane = {

    val outerBounds = variable(new Dimension(0, 0))
    val width = image.getWidth
    val height = image.getHeight
    val renderer = new ImageBuffer(image)

    val ui = new JScrollPane(
      ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
      ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    ui addComponentListener new ComponentAdapter() {
      override def componentResized(e : ComponentEvent) : Unit =
        outerBounds set ui.getViewport().getExtentSize()
    }


    val zoomFactor =
      effectiveZoomFor(image.getWidth, image.getHeight) _ ≻ outerBounds.behaviour ≻ zoom
    val targetSize =
      ImageBuffer.targetImageSize(image.getWidth, image.getHeight) _ ≻ zoomFactor
    val sizeGoal =
      preferredSizeFor _ ≻ outerBounds.behaviour ≻ targetSize


    val inner = new JComponent(){
      override def paintComponent(g : Graphics) : Unit = {
        super.paintComponents(g)
        val sz = this.getSize()
        renderer.renderTo(g, sz.width, sz.height, zoomFactor.value)
      }
    }

    sizeGoal ≺ (x ⇒ {
      inner.setPreferredSize(x)
      inner.revalidate()
      inner.repaint()
    })

    targetSize ≺ (x ⇒ inner.repaint())

    ui.getViewport.setView(inner)
    ui setBorder null

    Scrolls.scrollByDrag(ui.getViewport)

    new ImagePane(ui, zoomFactor)
  }



  /**
   * Calculates an image preferred size for specific image sizes and
   * scrollpane result.
   */
  private def preferredSizeFor(
        imageSize : Dimension)(
        viewportSize : Dimension)
      : Dimension =
    new Dimension(
      Math.max(viewportSize.width - 2, imageSize.width),
      Math.max(viewportSize.height - 2, imageSize.height))



  /**
   * Calculates an effective zoom factor for specific image sizes
   * and outer component width.
   */
  private def effectiveZoomFor(
        width : Int, height : Int)(
        viewportSize : Dimension)(
        zoom : Zoom)
      : Double =
    zoom match {
      case Zoom.Fixed(factor) ⇒ factor
      case Zoom.Fit ⇒
        Math.min(
          viewportSize.width.toDouble / width,
          viewportSize.height.toDouble / height)
      case Zoom.SmartFit ⇒
        Math.min(1.0,
          Math.min(
            viewportSize.width.toDouble / width,
            viewportSize.height.toDouble / height))
    }
}
