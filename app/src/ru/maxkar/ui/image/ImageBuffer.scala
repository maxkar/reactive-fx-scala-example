package ru.maxkar.ui.image

import java.awt.Graphics
import java.awt.Dimension
import java.awt.GraphicsEnvironment
import java.awt.image.BufferedImage
import java.awt.RenderingHints


/**
 * Buffer for resized image, image with effects, etc.
 * Provides caching of resulting image.
 */
private[image] class ImageBuffer(img : BufferedImage) {
  import ImageBuffer._



  /** Current image buffer. */
  private var buffer : BufferedImage =
    GRAPHIC_CONFIG.createCompatibleImage(img.getWidth, img.getHeight)



  /** Last zoom factor used for the image. */
  private var lastDims : Dimension = null



  /**
   * Renders this buffer into the specified graphics.
   * @param w destination width
   * @param d destination height
   */
  private[image] def renderTo(g : Graphics, w : Int, h : Int, zoom : Double) : Unit = {
    rezoomIfNeeded(zoom)

    val effectiveSize = lastDims
    val dx = (w - effectiveSize.width) / 2
    val dy = (h - effectiveSize.height) / 2


    val clip = g.getClipBounds()

    if (clip == null) {
      g.drawImage(buffer,
        dx, dy, dx + effectiveSize.width, dy + effectiveSize.height,
        0, 0, effectiveSize.width, effectiveSize.height,
      null)
      return
    }


    val minx = Math.max(clip.x, dx)
    val miny = Math.max(clip.y, dy)
    val maxx = Math.min(clip.x + clip.width, dx + lastDims.width)
    val maxy = Math.min(clip.y + clip.height, dy + lastDims.height)

    if (minx >= maxx || miny >= maxy)
      return

    g.drawImage(buffer,
      minx, miny, maxx, maxy,
      minx - dx, miny - dy, maxx - dx, maxy - dy,
      null)
  }



  /**
   * Rescales image if zoom changed.
   */
  private def rezoomIfNeeded(zoom : Double) : Unit = {
    val newDims = targetImageSize(img.getWidth, img.getHeight)(zoom)
    if (lastDims != null &&
        lastDims.width == newDims.width &&
        lastDims.height == newDims.height)
      return

    relocateBuffer(newDims)
    lastDims = newDims

    val g = buffer.createGraphics()
    g.setRenderingHint(
      RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.drawImage(img,
      0, 0, lastDims.width, lastDims.height,
      0, 0, img.getWidth, img.getHeight,
      null)
    g.dispose()
  }



  /** Relocates buffer if needed. */
  private def relocateBuffer(dims : Dimension) : Unit = {
    if (dims.width <= buffer.getWidth && dims.height <= buffer.getHeight)
      return

    buffer = GRAPHIC_CONFIG.createCompatibleImage(
      (dims.width * OVERSCAN_FACTOR).ceil.toInt,
      (dims.height * OVERSCAN_FACTOR).ceil.toInt)
  }


}



/** Buffer companion object. */
object ImageBuffer {
  /** Image grow factor used as a safety buffer. */
  private var OVERSCAN_FACTOR = 1.3



  /** Graphics configuration for default environment. */
  val GRAPHIC_CONFIG = {
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val device = env.getDefaultScreenDevice()
    device.getDefaultConfiguration
  }



  /**
   * Calculates target image size.
   */
  private[ui] def targetImageSize(
        width : Int, height : Int)(zoom : Double)
      : Dimension =
    new Dimension((width * zoom).floor.toInt, (height * zoom).floor.toInt)
}
