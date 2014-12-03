package ru.maxkar.widgets.vfs

import ru.maxkar.util.vfs._
import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._


import ru.maxkar.fx.Bridge._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import javafx.scene.Node
import javafx.scene.layout.Region
import javafx.scene.control.ListView
import javafx.scene.control.ListCell
import javafx.scene.image._

import ru.maxkar.util.vfs._


final class BrowserViewRenderer private(
    dirIcon : Image,
    archiveIcon : Image,
    imageIcon : Image,
    imageUnknown : Image) {
  import BrowserViewRenderer._


  /**
   * Converts directory view to item sequence.
   */
  private def toItems(view : DirectoryBrowser.Viewport) : Seq[Item] = {
    val basicItems = view.content.map(x ⇒ NestedEntry(x))
    view.parentAction match {
      case None ⇒ basicItems
      case Some(x) ⇒ ParentEntry(x) +: basicItems
    }
  }



  /**
   * Checks if name represents an image.
   */
  private def isImage(name : String) : Boolean =
    name.endsWith(".png") || name.endsWith(".jpg") || name.endsWith(".gif")



  /**
   * Find an icon for the entity.
   */
  private def iconOf(e : DirectoryEntry) : Image =
    if (e.container && e.filestream)
      archiveIcon
    else if (e.container)
      dirIcon
    else if (isImage(e.name))
      imageIcon
    else
      imageUnknown



  /**
   * Renders a list item.
   */
  private def renderItem(view : ListView[Item]) : ListCell[Item] =
    new ListCell[Item] {
      override def updateItem(item : Item, empty : Boolean) : Unit = {
        super.updateItem(item, empty)
        if (item == null)
          return
        item match {
          case ParentEntry(_) ⇒
            setGraphic(new ImageView(dirIcon))
            setText("..")
          case NestedEntry(e) ⇒
            setGraphic(new ImageView(iconOf(e)))
            setText(e.name)
        }
      }
    }



  /**
   * Renders a list view.
   */
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
    view.setCellFactory(renderItem _)
    view
  }
}

/**
 * File browser view.
 */
final object BrowserViewRenderer {
  /** Basic renderable and navigable item. */
  abstract sealed class Item



  /** "Parent directory" file. */
  final case class ParentEntry(nav : () ⇒ Unit) extends Item



  /** "Nested directory" file. */
  final case class NestedEntry(entry : DirectoryEntry) extends Item



  /**
   * Creates a new entry list reader with the specified
   * directory icons.
   */
  def make(
        dirIcon : Image = null,
        archiveIcon : Image = null,
        imageIcon : Image = null,
        imageUnknown : Image = null)
      : BrowserViewRenderer =
    new BrowserViewRenderer(dirIcon, archiveIcon, imageIcon, imageUnknown)
}
