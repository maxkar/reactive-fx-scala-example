package ru.maxkar.ui.image

import java.awt.Dimension
import java.awt.Point
import java.awt.image.BufferedImage

/**
 * Fragment of the image.
 * @param start fragment start point.
 * @param base base image.
 * @param fragment size.
 */
private[image] final class ImageFragment(
      val base : BufferedImage,
      val start : Point,
      val size : Dimension)
