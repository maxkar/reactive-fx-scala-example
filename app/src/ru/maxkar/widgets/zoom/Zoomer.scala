package ru.maxkar.widgets.zoom

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._



/**
 * Zoom object companion.
 */
object Zoomer {
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
      1 / (1 / scale - 0.5)
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
}
