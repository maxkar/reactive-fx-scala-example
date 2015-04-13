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

    val ui = new JScrollPane(
      ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
      ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)

    val outerBounds = Scrolls.viewportSize(ui)

    val zoomFactor = effectiveZoomFor(image) _ ≻ outerBounds ≻ zoom
    val scaler = new ScaleBuffer()
    val scaledState = (scaler.render _).curried(image) ≻ zoomFactor
    val sizeGoal = preferredSizeFor _ ≻ outerBounds ≻ scaledState

    val inner = new RendererComponent()
    (inner.updateContent _).curried ≻ scaledState ≻ sizeGoal

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
        viewportSize : Dimension)(
        scaledState : (_, Dimension))
      : Dimension =
    new Dimension(
      Math.max(viewportSize.width - 2, scaledState._2.width),
      Math.max(viewportSize.height - 2, scaledState._2.height))



  /**
   * Calculates an effective zoom for the image, selected zoom and actual
   * viewport size.
   */
  private def effectiveZoomFor(
        image : BufferedImage)(
        viewportSize : Dimension)(
        zoom : Zoom)
      : Double =
    zoom.scaleFor(
      image.getWidth, image.getHeight,
      viewportSize.getWidth, viewportSize.getHeight)
}
