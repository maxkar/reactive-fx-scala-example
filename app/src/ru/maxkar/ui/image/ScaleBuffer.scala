package ru.maxkar.ui.image

import java.awt.Graphics
import java.awt.Dimension
import java.awt.GraphicsEnvironment
import java.awt.image.BufferedImage
import java.awt.RenderingHints

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._




/**
 * Scaled image buffer. Helps to store scaled image instances in shared
 * (growing) image buffer. Optimizes rendering operation and at the same
 * time tries to reduce image footprint.
 */
private[image] final class ScaleBuffer  {
  import ScaleBuffer._


  /** Current image buffer. */
  private var buffer : BufferedImage =
    GRAPHIC_CONFIG.createCompatibleImage(
      DEFAULT_IMAGE_SIZE, DEFAULT_IMAGE_SIZE)



  /**
   * Renders an image with the given zoom and returns cached buffer and actual
   * (drawable) buffer dimensions.
   */
  def render(image : BufferedImage, zoom : Double) : (BufferedImage, Dimension) = {
    val realDims = targetImageSize(image.getWidth, image.getHeight, zoom)
    ensureSize(realDims)
    renderScaledImage(image, realDims)
    (buffer, realDims)
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



/** Buffer companion object. */
object ScaleBuffer {
  /** Image grow factor used as a safety buffer. */
  private var OVERSCAN_FACTOR = 1.3


  /** Default image/buffer size. */
  private val DEFAULT_IMAGE_SIZE = 1000


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

