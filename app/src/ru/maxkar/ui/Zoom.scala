package ru.maxkar.ui

import actions.ActionSpec

import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import java.awt.event.KeyListener
import java.awt.event.KeyAdapter

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import syntax._


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
      Math.min(Fit.scaleFor(origW, origH, targetW, targetH), 1.0)

    override def toString() : String = "Smart fit"
  }



  /**
   * Default mapping from keys to actions.
   */
  private val DEFAULT_KEY_BINDINGS = Seq(
    "PLUS" → "zoomIn",
    "ADD" → "zoomIn",
    "EQUALS" → "zoomIn",

    "MINUS" → "zoomOut",
    "UNDERSCORE" → "zoomOut",
    "SUBTRACT" → "zoomOut",
    "EQUALS" → "zoomOut",

    "DIVIDE" → "reset",
    "MULTIPLY" → "fit",

    "NUM_0" → "smartFit",
    "INSERT" → "smartFit"
  )



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
  def nextPresetZoom(presets : Seq[Double], scale : Double) : Zoom = {
    val naz = nextAutoZoom(scale)
    if (presets.isEmpty || naz < presets.head)
      return Fixed(naz)

    val iter = presets.iterator
    while (iter.hasNext) {
      val guess = iter.next
      if (guess > scale)
        return Fixed(guess)
    }

    return Fixed(naz)
  }



  /**
   * Calculates a previous zoom in a smart way. Tries to go to
   * a previous available item which is lesser than a current zoom.
   * If zoom is out of boutds, calculates an automatic zoom.
   * @param presets zoom presets. Must be sorted in ascending order.
   * @param scale current scale.
   */
  def prevPresetZoom(presets : Seq[Double], scale : Double) : Zoom = {
    val paz = prevAutoZoom(scale)
    if (presets.isEmpty || paz > presets.last)
      return Fixed(paz)


    val iter = presets.reverseIterator
    while (iter.hasNext) {
      val guess = iter.next
      if (guess < scale)
        return Fixed(guess)
    }

    return Fixed(paz)
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
   * Creates action definitions for the zoom model.
   * @param prefix action key prefix.
   * @param presets zoom presets.
   * @param effectiveZoom current(effective zoom) value
   * @param callback callback to invoke with new zoom.
   */
  def zoomActionsFor(
        prefix : String,
        presets : Seq[Double],
        effectiveZoom : Behaviour[Option[Double]],
        zoom : Behaviour[Zoom],
        callback : Zoom ⇒ Unit) :
      Seq[ActionSpec] =
    Seq(
      prefix + "zoomIn" :-> {
        subEffectiveZoom(effectiveZoom.value, zoom.value)match {
          case Some(x) ⇒ callback(nextPresetZoom(presets, x))
          case None ⇒ ()
        }
      },
      prefix + "zoomOut" :-> {
        subEffectiveZoom(effectiveZoom.value, zoom.value) match {
          case Some(x) ⇒ callback(prevPresetZoom(presets, x))
          case None ⇒ ()
        }
      },
      prefix + "reset" :->
        callback(Zoom.Fixed(1.0)),
      prefix + "fit" :->
        callback(Zoom.Fit),
      prefix + "smartFit" :->
        callback(Zoom.SmartFit)
    )



  /**
   * Generates key-to-action binding for the given action prefix.
   */
  def defaultKeyBindings(prefix : String) : Seq[(String, String)] =
    DEFAULT_KEY_BINDINGS.map(x ⇒ (x._1, prefix + x._2))
}
