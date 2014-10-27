package ru.maxkar.widgets.zoom

/**
 * Zoom factor definition.
 */
abstract sealed class Zoom {
  /**
   * Calculates zoom for given original size and destination
   * (available) size.
   */
  def scaleFor(
        origW : Double, origH : Double,
        targetW : Double, targetH : Double)
      : Double
}



/**
 * Zoom class companion.
 */
object Zoom {
  /** Zoom with a fixed scale. */
  final case class Fixed(scale : Double) extends Zoom {
    override def scaleFor(
          origW : Double, origH : Double,
          targetW : Double, targetH : Double)
        : Double =
      scale

    override def toString() : String = (scale * 100) + "%"
  }


  /** Fits an item to the screen. */
  case object Fit extends Zoom {
    override def scaleFor(
          origW : Double, origH : Double,
          targetW : Double, targetH : Double)
        : Double =
      Math.min(targetW / origW, targetH / origH)

    override def toString() : String = "Fit size"
  }


  /**
   * Smart fit where image is scaled down but could not scale up.
   */
  case object SmartFit extends Zoom {
    override def scaleFor(
          origW : Double, origH : Double,
          targetW : Double, targetH : Double)
        : Double =
      Math.min(Math.min(targetW / origW, targetH / origH), 1.0)

    override def toString() : String = "Smart fit"
  }
}
