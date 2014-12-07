package ru.maxkar.widgets.vfs

import ru.maxkar.fx.Bridge._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import javafx.scene.Node
import javafx.scene.layout.Region
import javafx.scene.control.ListView
import javafx.scene.control.ListCell
import javafx.scene.image._

/**
 * File walker renderer.
 */
object FileWalkerView {
  /**
   * Creates a new entry list reader with the specified
   * directory icons.
   */
  def make(
        walker : FileWalker,
        dirIcon : Image = null,
        archiveIcon : Image = null,
        imageIcon : Image = null,
        imageUnknown : Image = null)(
        implicit ctx : BindContext)
      : Region = {

    def iconForType(ft : FileType) : Image =
      ft match {
        case FileType.ParentDirectory | FileType.Directory ⇒ dirIcon
        case FileType.Container ⇒ archiveIcon
        case FileType.Image ⇒ imageIcon
        case _ ⇒ imageUnknown
      }

    def mkCell(view : ListView[FileInfo]) : ListCell[FileInfo] =
      new ListCell[FileInfo] {
        override def updateItem(item : FileInfo, empty : Boolean) : Unit = {
          super.updateItem(item, empty)
          if (item == null || empty) {
            setText(null)
            setGraphic(null)
            return
          }
          setText(item.name)
          setGraphic(new ImageView(iconForType(item.fileType)))
        }
      }

    val list = new ListView(walker.items)
    bindRO(list.getSelectionModel().selectedItemProperty(),
      (x : FileInfo) ⇒ list.getSelectionModel().select(x),
      walker.selection, walker.select)
    list.setCellFactory(mkCell _)

    list
  }
}
