package ru.maxkar.ui.vfs

import java.awt.image.BufferedImage
import java.awt.Component
import java.awt.Dimension
import java.awt.GridBagLayout
import java.awt.GridBagConstraints

import java.awt.event.FocusAdapter
import java.awt.event.FocusEvent

import javax.swing.JComponent
import javax.swing.JList
import javax.swing.JLabel
import javax.swing.JPanel
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


    val filter = variable("")
    val showFilter = variable(false)
    val filterUI = mkFilterUI(filter, filter.set, walker)
    val filteredItems = filterItems _ ≻ filter.behaviour ≻ walker.items

    val (list, listActions) = Controls.list(filteredItems, walker.selection, walker.select, cellRenderer)
    val res = new JScrollPane(list)

    filterUI.actions ++= Seq(
      "pgup" :-> listActions.pageUp(),
      "pgdn" :-> listActions.pageDown(),
      "next" :-> listActions.nextItem(),
      "prev" :-> listActions.prevItem(),
      "open" :-> Activator.batch {
        walker.open()
        filter set ""
        showFilter set false
      },
      "hide" :-> Activator.batch {
        filter set ""
        showFilter set false
      })

    filterUI.keysWhenFocusedAncestor ++= Seq(
      "UP" → "prev",
      "DOWN" → "next",
      "PAGE_UP" → "pgup",
      "PAGE_DOWN" → "pgdn",
      "ENTER" → "open",
      "ESCAPE" → "hide")

    res.actions ++= Seq(
      "quickselect" :->
        quickSelect(filteredItems, walker.selection, walker.select, res, walker.open),
      "enable_filter" :-> {
        showFilter set true
        filterUI.requestFocusInWindow()
      })
    res.keysWhenFocusedAncestor ++= Seq(
      "SLASH" → "quickselect",
      "ctrl F" → "enable_filter")

    mkUI(
      Controls.contentOf(showFilter.behaviour ≺ (x ⇒ if (x) filterUI else null)),
      res)
  }



  /** Creates a filter model and filter input UI. */
  private def mkFilterUI(
        filterValue : Behaviour[String],
        selector : String ⇒ Unit,
        walker : FileWalker)(
        implicit ctx : BindContext)
      : JComponent = {
    var lastValue = walker.selection.value

    Controls.input(filterValue, x ⇒ Activator.batch {
      val curSel = walker.selection.value
      if (curSel != null)
        if (filterMatches(x, curSel))
          lastValue = curSel
        else
          walker.select(null)
      else
        if (filterMatches(x, lastValue))
          walker.select(lastValue)

      selector(x)
    })
  }



  /** Filters items in the list. */
  private def filterItems(filter : String)(items : Seq[FileInfo]) : Seq[FileInfo] =
    if (filter == "")
      items
    else
      items.filter(x ⇒ filterMatches(filter, x))



  /** Checks if file matches filter data. */
  private def filterMatches(filter : String, item : FileInfo) : Boolean =
    item.name.indexOf(filter) >= 0



  /** Selects an item with the specific offest from the current item. */
  private def selectWithOffset(
        items : Behaviour[Seq[FileInfo]],
        selection : Behaviour[FileInfo],
        selector : FileInfo ⇒ Unit,
        offset : Int) : Unit = {
    val v = selection.value()
    if (v == null)
      return
    val witems = items.value()
    val cidx = witems.indexOf(v)
    if (cidx < 0)
      return
    val newIdx = cidx + offset
    if (0 <= newIdx && newIdx < witems.length )
      selector(witems(newIdx))
  }



  /** Creates vertical component list. */
  private def mkUI(filter : JComponent, body : JComponent) : JComponent = {
    val res = new JPanel()
    res.setLayout(new GridBagLayout())

    val gbc = new GridBagConstraints()
    gbc.weightx = 1
    gbc.weighty = 0
    gbc.gridx = 0
    gbc.gridy = 0
    gbc.fill = GridBagConstraints.HORIZONTAL
    gbc.anchor = GridBagConstraints.NORTH

    res.add(filter, gbc)
    gbc.gridy += 1
    gbc.weighty = 1
    gbc.fill = GridBagConstraints.BOTH
    res.add(body, gbc)

    res
  }


  /** Shows a quick select popup and performs a selection. */
  private def quickSelect(
        items : Behaviour[Seq[FileInfo]],
        selection : Behaviour[FileInfo],
        selector : FileInfo ⇒ Unit,
        parent : JComponent,
        acceptor : () ⇒ Unit)
      : Unit = {
    implicit val ctx = permanentBind

    val initialSelect = selection.value()
    val valid = variable(true)

    val filter = variable("")
    val res = Controls.input(filter, nv ⇒ Activator.batch {
      filter set nv

      if (nv == "")
        valid set true
      else
        items.value().find(x ⇒ x.name.startsWith(nv)) match {
          case None ⇒ valid set false
          case Some(x) ⇒
            selector(x)
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
        if (selection.value() != null)
          acceptor()
      },
      "approve_next" :-> {
        popup.hide()
        selectWithOffset(items, selection, selector, 1)
      },
      "approve_prev" :-> {
        popup.hide()
        selectWithOffset(items, selection, selector, -1)
      },
      "cancel" :-> {
        popup.hide()
        if (initialSelect != null)
          selector(initialSelect)
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
