package ru.maxkar.widgets.vfs

import ru.maxkar.fx.Bridge._

import javafx.beans.value._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import javafx.scene.Node
import javafx.scene.layout.Region
import javafx.scene.control.ListView
import javafx.scene.control.ListCell
import javafx.scene.image._
import javafx.collections.FXCollections
import scala.collection.JavaConverters._

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

    val ilist = FXCollections.observableArrayList[FileInfo](walker.items.value.asJava)
    val list = new ListView(ilist)
    list.setCellFactory(mkCell _)

    var updating = false

    def toFX(items : Seq[FileInfo])(sel : FileInfo) : Unit = {
      updating = true

      try {
        if (walker.items.change.value) {
          ilist.setAll(items.asJava)
        }

        if (walker.items.change.value || walker.selection.change.value) {
          val selmodel = list.getSelectionModel()
          if (selmodel.getSelectedItem() != sel)
            selmodel.select(sel)

          if (walker.items.change.value) {
            if (sel != null) {
              val idx = list.getSelectionModel().getSelectedIndex()
              println(idx)
              if (idx < items.length - 1)
                list.scrollTo(idx + 1)
              if (idx > 0)
                list.scrollTo(idx - 1)
              list.scrollTo(sel)
            } else
              list.scrollTo(0)
          }
        }
      } finally {
        updating = false
      }
    }
    toFX _ ≻ walker.items ≻ walker.selection

    val listener = new ChangeListener[FileInfo] {
      override def changed(v : ObservableValue[_ <: FileInfo],
            oldValue : FileInfo, newValue : FileInfo)
          : Unit =
        if (!updating && walker.selection.value != newValue)
          walker.select(newValue)
    }
    list.getSelectionModel().selectedItemProperty().addListener(listener)
    ctx.lifespan.onDispose(() ⇒
      list.getSelectionModel().selectedItemProperty().removeListener(listener))


    list
  }
}
