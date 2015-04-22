package ru.maxkar.ui.vfs

import java.awt.image.BufferedImage
import java.awt.Component
import java.awt.Dimension

import java.awt.event.FocusAdapter
import java.awt.event.FocusEvent

import javax.swing.JComponent
import javax.swing.JList
import javax.swing.JLabel
import javax.swing.JScrollPane
import javax.swing.DefaultListCellRenderer
import javax.swing.ListCellRenderer
import javax.swing.ImageIcon
import javax.swing.PopupFactory

import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.proc.Activator

import ru.maxkar.ui.syntax._
import ru.maxkar.ui.actions.ActionSpec

import ru.maxkar.ui.Layouts
import ru.maxkar.ui.Controls


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

    val baseCellRenderer = new DefaultListCellRenderer()
    val cellRenderer = new ListCellRenderer[FileInfo] {
      override def getListCellRendererComponent(
            list : JList[_ <: FileInfo], value : FileInfo, idx : Int,
            isSelected : Boolean, hasFocus : Boolean) : Component = {
        val label =
          baseCellRenderer.getListCellRendererComponent(
            list, value : Any, idx, isSelected, hasFocus)
          .asInstanceOf[JLabel]
        label.setText(value.name)
        label.setIcon(new ImageIcon(iconForType(value.fileType)))
        label
      }


      def iconForType(ft : FileType) : BufferedImage =
        ft match {
          case FileType.ParentDirectory | FileType.Directory ⇒ dirIcon
          case FileType.Container ⇒ archiveIcon
          case FileType.Image ⇒ imageIcon
          case _ ⇒ imageUnknown
        }
    }


    val list = Controls.list(walker.items, walker.selection, walker.select, cellRenderer)
    val res = new JScrollPane(list)

    res.actions ++= Seq("quickselect" :-> quickSelect(walker, res))
    res.keysWhenFocusedAncestor ++= Seq("SLASH" → "quickselect")

    res
  }



  /** Selects an item with the specific offest from the current item. */
  private def selectWithOffset(walker : FileWalker, offset : Int) : Unit = {
    val v = walker.selection.value()
    if (v == null)
      return
    val witems = walker.items.value()
    val cidx = witems.indexOf(v)
    if (cidx < 0)
      return
    val newIdx = cidx + offset
    if (0 <= newIdx && newIdx < witems.length )
      walker.select(witems(newIdx))
  }



  /** Shows a quick select popup and performs a selection. */
  private def quickSelect(walker : FileWalker, parent : JComponent) : Unit = {
    implicit val ctx = permanentBind

    val initialSelect = walker.selection.value()
    val valid = variable(true)

    val filter = variable("")
    val res = Controls.input(filter, nv ⇒ Activator.batch {
      filter set nv

      if (nv == "")
        valid set true
      else
        walker.items.value().find(x ⇒ x.name.startsWith(nv)) match {
          case None ⇒ valid set false
          case Some(x) ⇒
            walker.select(x)
            valid set true
        }
    })

    val defaultColor = res.getBackground()
    val itemColor = valid.behaviour ≺ (v ⇒ if (v) defaultColor else java.awt.Color.PINK)
    itemColor ≺ res.setBackground

    res setColumns 10

    val loc = parent.getLocationOnScreen()
    val ppf = PopupFactory.getSharedInstance()
    val popup = ppf.getPopup(parent, res, loc.x, loc.y + parent.getHeight)
    popup.show()
    res.requestFocus()

    res addFocusListener new FocusAdapter() {
      override def focusLost(e : FocusEvent) : Unit =
        popup.hide()
    }

    res.actions ++= Seq(
      "approve" :-> popup.hide(),
      "open" :-> {
        popup.hide()
        if (walker.selection.value() != null)
          walker.open()
      },
      "approve_next" :-> {
        popup.hide()
        selectWithOffset(walker, 1)
      },
      "approve_prev" :-> {
        popup.hide()
        selectWithOffset(walker, -1)
      },
      "cancel" :-> {
        popup.hide()
        if (initialSelect != null)
          walker.select(initialSelect)
      })

    res.keysWhenFocusedAncestor ++= Seq(
      "ENTER" → "open",
      "ESCAPE" → "cancel",
      "UP" → "approve_prev",
      "DOWN" → "approve_next",
      "ctrl ENTER" → "approve")
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
