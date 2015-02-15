package ru.maxkar.ui.vfs

import java.awt.image.BufferedImage
import java.awt.Component

import javax.swing.JComponent
import javax.swing.JList
import javax.swing.JLabel
import javax.swing.JScrollPane
import javax.swing.DefaultListCellRenderer
import javax.swing.ListCellRenderer
import javax.swing.ImageIcon

import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import ru.maxkar.ui.syntax._
import ru.maxkar.ui.actions.ActionSpec


/**
 * File walker renderer.
 */
object FileWalkerView {
  /**
   * Default mapping between action keys and action handlers.
   */
  private val DEFAULT_ACTION_MAP = Seq(
    "ENTER" → "open",
    "BACK_SPACE" → "up"
  )



  /**
   * Creates a new entry list reader with the specified
   * directory icons.
   */
  def make(
        walker : FileWalker,
        dirIcon : BufferedImage = null,
        archiveIcon : BufferedImage = null,
        imageIcon : BufferedImage = null,
        imageUnknown : BufferedImage = null)(
        implicit ctx : BindContext)
      : JComponent = {

    val cellRenderer = new DefaultListCellRenderer()

    def iconForType(ft : FileType) : BufferedImage =
      ft match {
        case FileType.ParentDirectory | FileType.Directory ⇒ dirIcon
        case FileType.Container ⇒ archiveIcon
        case FileType.Image ⇒ imageIcon
        case _ ⇒ imageUnknown
      }


    var updating = false
    val list = new JList[FileInfo](walker.items.value.toArray)

    def syncModels(items : Seq[FileInfo], sel : FileInfo) : Unit = {
      updating = true
      try {
        val lc = walker.items.change.value
        val sc = walker.selection.change.value

        if (lc)
          list.setListData(walker.items.value.toArray)

        if (lc || sc)
          list.setSelectedValue(walker.selection.value(), true)

        val si = list.getSelectedIndex()
        if (si > 0)
          list.ensureIndexIsVisible(si - 1)
        if (si < items.length - 1)
          list.ensureIndexIsVisible(si + 1)
        list.ensureIndexIsVisible(si)

      }  finally {
        updating = false
      }
    }


    (syncModels _).curried ≻ walker.items ≻ walker.selection
    if (walker.selection.value != null)
      list.setSelectedValue(walker.selection.value, true)


    list addListSelectionListener new ListSelectionListener() {
      override def valueChanged(e : ListSelectionEvent) : Unit = {
        if (updating)
          return

        val idx = list.getSelectedIndex()
        walker.select(
          if (idx < 0)
            null
          else
            walker.items.value()(idx))
      }
    }


    list setCellRenderer new ListCellRenderer[FileInfo] {
      override def getListCellRendererComponent(
            list : JList[_ <: FileInfo], value : FileInfo, idx : Int,
            isSelected : Boolean, hasFocus : Boolean) : Component = {
        val label =
          cellRenderer.getListCellRendererComponent(
            list, value : Any, idx, isSelected, hasFocus)
          .asInstanceOf[JLabel]
        label.setText(value.name)
        label.setIcon(new ImageIcon(iconForType(value.fileType)))
        label
      }
    }

    val res = new JScrollPane(list)
    res
  }



  /**
   * Returns navigation actions for the specific file walker.
   * Supported actions: <ul>
   *   <li>open - open current selection
   *   <li>up - go one level up
   * </ul>
   * @param prefix action name prefix.
   */
  def navActionsFor(prefix : String, w : FileWalker) : Seq[ActionSpec] =
    Seq(
      prefix + "open" :-> w.open,
      prefix + "up" :-> {
        if (w.items.value.contains(FileInfo.ParentDirectory)) {
          w.select(FileInfo.ParentDirectory)
          w.open()
        }
      }
    )


  /**
   * Creates default key bindings for actions with specific prefix.
   */
  def defaultKeyBindings(prefix : String) : Seq[(String, String)] =
    DEFAULT_ACTION_MAP.map(x ⇒ (x._1, prefix + x._2))
}
