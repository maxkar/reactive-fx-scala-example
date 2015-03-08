package ru.maxkar.ui.image

import java.io.File
import java.awt.image.BufferedImage


/**
 * Image loader utility.
 */
object ImageLoader {
  /** File load implementation. */
  def loadFile(file : File) : BufferedImage =
    if (file == null)
      null
    else
      javax.imageio.ImageIO.read(file)
}
