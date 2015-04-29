package ru.maxkar.ui.vfs

import java.awt.event.FocusAdapter
import java.awt.event.FocusEvent

import javax.swing.JComponent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.proc.Activator

import ru.maxkar.ui.syntax._
import ru.maxkar.ui.Controls


/** Filter data input.  */
private[vfs] final class FilterInput(implicit ctx : BindContext) {
  import FilterInput._


  /** Filter is enabled at this moment. */
  private val enabledV = variable(false)


  /** Flag indicating that filter is enabled. */
  val enabled = enabledV.behaviour


  /** Filtering string. */
  private val filterStr = variable("")


  /** Filtering function for the current filter configuration. */
  val filterFn = mkFilterFn _ ≻ enabled ≻ filterStr.behaviour


  /** Filter input component. */
  private val inputComponent = {
    val res = Controls.input(filterStr, filterStr.set)
    res addFocusListener new FocusAdapter() {
      override def focusLost(e : FocusEvent) : Unit = disable()
    }
    res
  }


  /** Filter component. */
  val ui = Controls.contentOf(getUIComponent(inputComponent) _ ≻ enabled)


  /** Activate filtering. */
  def enable() : Unit = {
    enabledV set true
    inputComponent.requestFocusInWindow
  }


  /** Disables filtering. */
  def disable() : Unit =
    if (enabledV.latestValue)
      Activator.batch {
        enabledV set false
        filterStr set ""
      }


  /** Resets filter to a default value. */
  def reset() : Unit = filterStr set ""
}



/** Filter input factory. */
private[vfs] final object FilterInput {
  /**
   * Creates a new filter input component.
   * Initially filter is inactive (disabled).
   */
  def create(implicit ctx : BindContext) : FilterInput = new FilterInput


  /**
   * Creates a filtering function.
   * @param enabled "filter enabled" function.
   * @param filterString filtering request.
   */
  def mkFilterFn(
        enabled : Boolean)(
        filterString : String)
      : FileInfo ⇒ Boolean = {
    if (!enabled || filterString == "")
      return acceptAll
    val fmatch = filterString.toLowerCase
    info ⇒ info.name.toLowerCase.indexOf(fmatch) >= 0
  }



  /** Accepts all items. */
  private def acceptAll(info : FileInfo) : Boolean = true


  /** Returns UI component to display. */
  private def getUIComponent(input : JComponent)(enabled : Boolean) : JComponent =
    if (enabled) input else null
}
