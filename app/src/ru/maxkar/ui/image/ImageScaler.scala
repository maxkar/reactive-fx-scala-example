package ru.maxkar.ui.image

import java.awt.Graphics
import java.awt.Dimension
import java.awt.Point
import java.awt.GraphicsEnvironment
import java.awt.image.BufferedImage
import java.awt.RenderingHints

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import ru.maxkar.ui.Zoom

/**
 * Scaled image buffer. Helps to store scaled image instances in shared
 * (growing) image buffer. Optimizes rendering operation and at the same
 * time tries to reduce image footprint.
 */
private final class ImageScaler  {
  import ImageScaler._


  /** Current image buffer. */
  private var buffer : BufferedImage =
    GRAPHIC_CONFIG.createCompatibleImage(
      DEFAULT_IMAGE_SIZE, DEFAULT_IMAGE_SIZE)



  /**
   * Renders an image with the given zoom and returns cached buffer and actual
   * (drawable) buffer dimensions.
   */
  def render(image : BufferedImage, zoom : Option[Double]) : ImageFragment = {
    if (image == null || zoom.isEmpty)
      return new ImageFragment(buffer, new Point(0, 0), new Dimension(0, 0))

    val realDims = targetImageSize(image.getWidth, image.getHeight, zoom.get)
    ensureSize(realDims)
    renderScaledImage(image, realDims)
    new ImageFragment(buffer, new Point(0, 0), realDims)
  }



  /** Renders image into the current buffer. Buffer should have sufficient size. */
  private def renderScaledImage(image : BufferedImage, destSize : Dimension) : Unit = {
    val g = buffer.createGraphics()
    g.setRenderingHint(
      RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.drawImage(image,
      0, 0, destSize.width, destSize.height,
      0, 0, image.getWidth, image.getHeight,
      null)
    g.dispose()
  }



  /** Ensures buffer size and reallocates it if it does not have enough space. */
  private def ensureSize(dims : Dimension) : Unit = {
    if (dims.width <= buffer.getWidth && dims.height <= buffer.getHeight)
      return

    val defWidth = Math.max(dims.width, buffer.getWidth)
    val defHeight = Math.max(dims.height, buffer.getHeight)

    buffer = GRAPHIC_CONFIG.createCompatibleImage(
      (defWidth * OVERSCAN_FACTOR).ceil.toInt,
      (defHeight* OVERSCAN_FACTOR).ceil.toInt)
  }
}




/** Image scaling utilities. */
private[image] final object ImageScaler {
  /** Image grow factor used as a safety buffer. */
  private var OVERSCAN_FACTOR = 1.3


  /** Default image/buffer size. */
  private val DEFAULT_IMAGE_SIZE = 1000


  /**
   * Scales an image for the given zoom and destination size.
   * @param image current image to scale.
   * @param zoom desired zoom.
   * @param destSize destination size (for auto zoom).
   * @return scaled image (as a fragment of other image) and
   *   actual zoom factor. Actual zoom is none only if image
   *   value is <code>null</code>.
   */
  def scale(
        image : Behaviour[BufferedImage],
        zoom : Behaviour[Zoom],
        destSize : Behaviour[Dimension])(
        implicit ctx : BindContext)
      : (Behaviour[ImageFragment], Behaviour[Option[Double]]) = {

    val effectiveZoom = effectiveZoomFor _ ≻ image ≻ destSize ≻ zoom
    val buffer = new ImageScaler()
    val img = (buffer.render _).curried ≻ image ≻ effectiveZoom
    (img, effectiveZoom)
  }



  /**
   * Calculates an effective zoom for the image, selected zoom and actual
   * viewport size.
   */
  private def effectiveZoomFor(
        image : BufferedImage)(
        viewportSize : Dimension)(
        zoom : Zoom)
      : Option[Double] =
    if (image == null)
      None
    else
      Some(zoom.scaleFor(
        image.getWidth, image.getHeight,
        viewportSize.getWidth, viewportSize.getHeight))



  /** Graphics configuration for default environment. */
  private val GRAPHIC_CONFIG = {
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val device = env.getDefaultScreenDevice()
    device.getDefaultConfiguration
  }



  /** Calculates target image size.  */
  private def targetImageSize(
        width : Int, height : Int, zoom : Double)
      : Dimension =
    new Dimension((width * zoom).ceil.toInt, (height * zoom).ceil.toInt)
}
