package ru.maxkar.ui.vfs

import java.awt.Color
import javax.swing.JComponent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.proc.Activator

import ru.maxkar.ui.Controls


/** UI for quick item selection. */
private[vfs] final class QuickNav(
      items  :Behaviour[Seq[FileInfo]])(
      implicit ctx : BindContext) {
  import QuickNav._

  /** If this navigation component is active. */
  private val enabled = variable(false)


  /** Current selection prefix. */
  private val prefix = variable("")


  /** Checks if filter is in effect. */
  val effective = isEffective _ ≻ enabled.behaviour ≻ prefix.behaviour


  /** Selected item. Value is null when no items was found. */
  val selectedItem = findItem _ ≻ items ≻ effective ≻ prefix.behaviour


  /** Checks if filter is valid in current state. */
  private val valid = isValid _ ≻ selectedItem ≻ effective


  /** Nav UI. */
  val ui : JComponent = {
    val res = Controls.input(prefix, prefix.set)
    res setColumns 10
    valid ≺ (x ⇒ if (x) Color.BLACK else Color.RED) ≺ res.setForeground
    res
  }



  /** Activates this quick nav. */
  def activate() : Unit = Activator.batch {
    enabled set true
    prefix set ""
    ui.requestFocusInWindow()
  }



  /** Deactivates this quick navigation. */
  def deactivate() : Unit = Activator.batch {
    enabled set false
    prefix set ""
  }
}



/** Quick selector companion. */
private[vfs] final object QuickNav {
  /**
   * Creates a new quick selection object.
   * @param items items to select from.
   */
  def create(
        items : Behaviour[Seq[FileInfo]])(
        implicit ctx : BindContext)
      : QuickNav =
    new QuickNav(items)



  /** Checks if quick nav is valid for the selection and effectiveness. */
  def isValid(selection : FileInfo)(effective : Boolean) : Boolean =
    !effective || selection != null



  /** Checks if nav should be in effect in the current state. */
  def isEffective(enabled : Boolean)(prefix : String) : Boolean =
    enabled && prefix != ""



  /** Searches for an item matching given condition.
   */
  def findItem(
        items : Seq[FileInfo])(
        effective : Boolean)(
        prefix : String)
      : FileInfo = {
    if (!effective)
      return null

    items.find(x ⇒ x.name.startsWith(prefix)).getOrElse(null)
  }
}
