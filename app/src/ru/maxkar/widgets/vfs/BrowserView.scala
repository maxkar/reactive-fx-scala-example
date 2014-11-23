package ru.maxkar.widgets.vfs

import ru.maxkar.util.vfs._
import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._


import ru.maxkar.fx.Bridge._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import javafx.scene.layout.Region
import javafx.scene.control.ListView

import ru.maxkar.util.vfs._

/**
 * File browser view.
 */
final object BrowserView {
  /** Basic renderable and navigable item. */
  abstract sealed class Item



  /** "Parent directory" file. */
  final case class ParentEntry(nav : () ⇒ Unit) extends Item {
    override def toString() : String = ".."
  }



  /** "Nested directory" file. */
  final case class NestedEntry(entry : DirectoryEntry) extends Item {
    override def toString() : String = entry.name
  }



  /**
   * Converts directory view to item sequence.
   */
  def toItems(view : DirectoryBrowser.Viewport) : Seq[Item] = {
    val basicItems = view.content.map(x ⇒ NestedEntry(x))
    view.parentAction match {
      case None ⇒ basicItems
      case Some(x) ⇒ ParentEntry(x) +: basicItems
    }
  }



  def render(
        list : DirectoryBrowser.Viewport,
        current : Behaviour[Item],
        selector : Item ⇒ Unit)(
        implicit ctx : BindContext)
      : Region = {
    val view = new ListView(toItems(list).asFXList)
    bindRO(
      view.getSelectionModel().selectedItemProperty(),
      (x : Item) ⇒ view.getSelectionModel().select(x),
      current, selector)
    view
  }
}
