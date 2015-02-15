package ru.maxkar.ui

import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import java.awt.event.KeyListener
import java.awt.event.KeyAdapter

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


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
        event : KeyEvent)
      : Option[Zoom] = {
    val czoom = subEffectiveZoom(effectiveZoom, selectedZoom)
    event.getKeyCode() match {
      case KeyEvent.VK_PLUS | KeyEvent.VK_ADD ⇒
        czoom.map(z ⇒ Zoom.Fixed(nextPresetZoom(presets, z)))
      case KeyEvent.VK_MINUS | KeyEvent.VK_UNDERSCORE | KeyEvent.VK_SUBTRACT | KeyEvent.VK_EQUALS ⇒
        czoom.map(z ⇒ Zoom.Fixed(prevPresetZoom(presets, z)))
      case KeyEvent.VK_DIVIDE ⇒ Some(Zoom.Fixed(1.0))
      case KeyEvent.VK_MULTIPLY ⇒ Some(Zoom.Fit)
      case KeyEvent.VK_INSERT ⇒ Some(Zoom.SmartFit)
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
      : KeyListener =
    new KeyAdapter() {
      override def keyPressed(evt : KeyEvent) {
        if (isModifierDown(evt))
          return

        evt.getKeyCode() match {
          case KeyEvent.VK_PLUS | KeyEvent.VK_ADD ⇒
            callback(Zoom.Fixed(
              nextPresetZoom(presets, effectiveZoom.value)))
            evt.consume()
          case KeyEvent.VK_MINUS | KeyEvent.VK_UNDERSCORE | KeyEvent.VK_SUBTRACT | KeyEvent.VK_EQUALS ⇒
            callback(Zoom.Fixed(
              prevPresetZoom(presets, effectiveZoom.value)))
            evt.consume()
          case KeyEvent.VK_DIVIDE ⇒
            callback(Zoom.Fixed(1.0))
            evt.consume()
          case KeyEvent.VK_MULTIPLY ⇒
            callback(Zoom.Fit)
            evt.consume()
          case KeyEvent.VK_INSERT ⇒
            callback(Zoom.SmartFit)
            evt.consume()
          case _ ⇒ ()
        }
      }
    }



  private def isModifierDown(evt : KeyEvent) : Boolean = {
    val mask =
      InputEvent.ALT_DOWN_MASK |
      InputEvent.ALT_GRAPH_DOWN_MASK |
      InputEvent.CTRL_DOWN_MASK |
      InputEvent.META_DOWN_MASK |
      InputEvent.SHIFT_DOWN_MASK
    return (evt.getModifiers() & mask) == 0
  }
}
