package ru.maxkar.widgets.zoom

import javafx.scene.input._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import ru.maxkar.fx.Events


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

    override def toString() : String = "%2.2f%%".format(scale * 100)
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



  /**
   * Calculates a next zoom factor for the specified current value.
   */
  def nextAutoZoom(scale : Double) : Double = {
    if (scale >= 1)
      scale + 0.5
    else
      1 / (1 / scale - 0.5)
  }



  /**
   * Calculates a previous zoom factor for the specified current value.
   */
  def prevAutoZoom(scale : Double) : Double = {
    if (scale > 1)
      scale - 0.5
    else
      1 / (1 / scale + 0.5)
  }



  /**
   * Calculates a next zoom in a smart way. Tries to go to a next
   * available item which is greater than a current zoom. If zoom
   * is out of bounds, calculates an automatic zoom.
   * @param presets zoom presets. Must be sorted in ascending order.
   * @param scale current scale.
   */
  def nextPresetZoom(presets : Seq[Double], scale : Double) : Double = {
    val naz = nextAutoZoom(scale)
    if (presets.isEmpty || naz < presets.head)
      return naz

    val iter = presets.iterator
    while (iter.hasNext) {
      val guess = iter.next
      if (guess > scale)
        return guess
    }

    return naz
  }



  /**
   * Calculates a previous zoom in a smart way. Tries to go to
   * a previous available item which is lesser than a current zoom.
   * If zoom is out of boutds, calculates an automatic zoom.
   * @param presets zoom presets. Must be sorted in ascending order.
   * @param scale current scale.
   */
  def prevPresetZoom(presets : Seq[Double], scale : Double) : Double = {
    val paz = prevAutoZoom(scale)
    if (presets.isEmpty || paz > presets.last)
      return paz


    val iter = presets.reverseIterator
    while (iter.hasNext) {
      val guess = iter.next
      if (guess < scale)
        return guess
    }

    return paz
  }



  /**
   * Calculates a new zoom in the effect.
   */
  private def subEffectiveZoom(
        x : Option[Double],
        cur : Zoom)
      : Option[Double] =
    x match {
      case Some(_) ⇒ x
      case None ⇒
        cur match {
          case Fixed(t) ⇒ Some(t)
          case _ ⇒ None
        }
    }



  /**
   * Finds a new zoom for the shortcut.
   */
  def zoomForShortcut(
        presets : Seq[Double],
        effectiveZoom : Option[Double],
        selectedZoom : Zoom,
        code : KeyCode)
      : Option[Zoom] = {
    val czoom = subEffectiveZoom(effectiveZoom, selectedZoom)
    code match {
      case KeyCode.PLUS | KeyCode.ADD ⇒
        czoom.map(z ⇒ Zoom.Fixed(nextPresetZoom(presets, z)))
      case KeyCode.MINUS | KeyCode.UNDERSCORE | KeyCode.SUBTRACT | KeyCode.EQUALS ⇒
        czoom.map(z ⇒ Zoom.Fixed(prevPresetZoom(presets, z)))
      case KeyCode.DIVIDE ⇒ Some(Zoom.Fixed(1.0))
      case KeyCode.MULTIPLY ⇒ Some(Zoom.Fit)
      case KeyCode.INSERT ⇒ Some(Zoom.SmartFit)
      case _ ⇒ None
    }
  }



  /**
   * Creates a zoom shortcut handler.
   * @param presets zoom preset values.
   * @param effectiveZoom effective (current) zoom value.
   * @param callback callback to invoke on a new zoom.
   */
  def zoomShortcutHandler(
        presets : Seq[Double],
        effectiveZoom : Behaviour[Double],
        callback : Zoom ⇒ Unit)
      : KeyEvent ⇒ Unit = {

    def res(evt : KeyEvent) {
      if (Events.modifierDown(evt))
        return

      evt.getCode match {
        case KeyCode.PLUS | KeyCode.ADD ⇒
          callback(Zoom.Fixed(
            nextPresetZoom(presets, effectiveZoom.value)))
          evt.consume()
        case KeyCode.MINUS | KeyCode.UNDERSCORE | KeyCode.SUBTRACT | KeyCode.EQUALS ⇒
          callback(Zoom.Fixed(
            prevPresetZoom(presets, effectiveZoom.value)))
          evt.consume()
        case KeyCode.DIVIDE ⇒
          callback(Zoom.Fixed(1.0))
          evt.consume()
        case KeyCode.MULTIPLY ⇒
          callback(Zoom.Fit)
          evt.consume()
        case KeyCode.INSERT ⇒
          callback(Zoom.SmartFit)
          evt.consume()
        case _ ⇒ ()
      }
    }

    res
  }
}
