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
import javax.swing.Popup

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
    "BACK_SPACE" → "up",
    "CONTEXT_MENU" → "crossnav",
    "ctrl CONTEXT_MENU" → "siblingnav"
  )



  /**
   * Creates a new entry list reader with the specified
   * directory icons.
   * @return ui component and "have valid selection" state.
   */
  def make(
        walker : FileWalker,
        dirIcon : BufferedImage = null,
        archiveIcon : BufferedImage = null,
        imageIcon : BufferedImage = null,
        imageUnknown : BufferedImage = null)(
        implicit ctx : BindContext)
      : (JComponent, Behaviour[Boolean]) = {

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

    val filter = FilterInput.create
    val filteredItems = filterItems _ ≻ filter.filterFn ≻ walker.items

    val qnav = QuickNav.create(filteredItems)
    var qnavPopup : Popup = null
    def hideQNav() : Unit =
      if (qnavPopup != null) {
        qnav.deactivate()
        qnavPopup.hide()
        qnavPopup = null
      }

    val visSelector = getUISelection _ ≻
      walker.selection ≻ filter.filterFn ≻
      qnav.effective ≻ qnav.selectedItem

    val canNav = visSelector ≺ (x ⇒ x != null)

    val (list, listActions) = Controls.list(filteredItems, visSelector, walker.select, cellRenderer)
    val res = new JScrollPane(list)

    val sorter = sort.SortUI.make(walker)

    /* Filter Actions. */
    filter.ui.actions ++= Seq(
      "pgup" :-> listActions.pageUp(),
      "pgdn" :-> listActions.pageDown(),
      "next" :-> listActions.nextItem(),
      "prev" :-> listActions.prevItem(),
      "open" :-> Activator.batch {
        walker.openItem(visSelector.value)
        filter.disable()
      },
      "back" :-> walker.openItem(FileInfo.ParentDirectory),
      "hide" :-> filter.disable()
    )

    filter.ui.keysWhenFocusedAncestor ++= Seq(
      "UP" → "prev",
      "DOWN" → "next",
      "PAGE_UP" → "pgup",
      "PAGE_DOWN" → "pgdn",
      "ENTER" → "open",
      "ESCAPE" → "hide",
      "alt LEFT" → "back",
      "alt RIGHT" → "open")


    /* Qnav actions. */
    qnav.ui.actions ++= Seq(
      "approve" :-> Activator.batch {
        if (qnav.selectedItem.value != null)
          walker.select(qnav.selectedItem.value)
        hideQNav()
      },
      "open" :-> Activator.batch {
        walker.openItem(qnav.selectedItem.value)
        hideQNav()
      },
      "approve_next" :-> Activator.batch {
        selectWithOffset(walker, qnav.selectedItem.value, 1)
        hideQNav()
      },
      "approve_prev" :-> Activator.batch {
        selectWithOffset(walker, qnav.selectedItem.value, -1)
        hideQNav()
      },
      "cancel" :-> hideQNav())

    qnav.ui.keysWhenFocusedAncestor ++= Seq(
      "ENTER" → "open",
      "ESCAPE" → "cancel",
      "UP" → "approve_prev",
      "DOWN" → "approve_next",
      "ctrl ENTER" → "approve")

    qnav.ui addFocusListener new FocusAdapter() {
      override def focusLost(e : FocusEvent) : Unit = hideQNav
    }


    /* List/result actions. */
    val ppf = PopupFactory.getSharedInstance()
    res.actions ++= Seq(
      "quickselect" :-> {
        val loc = res.getLocationOnScreen()
        qnavPopup = ppf.getPopup(res, qnav.ui, loc.x, loc.y + res.getHeight)
        qnavPopup.show()
        qnav.activate()
      },
      "enable_filter" :-> filter.enable,
      "toggle_sort" :-> sorter.toggle)
    res.keysWhenFocusedAncestor ++= Seq(
      "SLASH" → "quickselect",
      "ctrl F" → "enable_filter",
      "ctrl S" → "toggle_sort")

    (mkUI(sorter.ui, filter.ui, res), canNav)
  }


  /** Calculates UI item to highlight. */
  private def getUISelection(
        base : FileInfo)(filter : FileInfo ⇒ Boolean)(
        qselActive : Boolean)(qselItem : FileInfo)
      : FileInfo =
    if (qselActive && qselItem != null)
      qselItem
    else if (filter(base))
      base
    else
      null



  /** Filters items in the list. */
  private def filterItems(filterFn : FileInfo ⇒ Boolean)(items : Seq[FileInfo]) : Seq[FileInfo] =
      items.filter(filterFn)



  /** Selects an item with the specific offest from the current item. */
  private def selectWithOffset(
        walker : FileWalker,
        selection : FileInfo,
        offset : Int) : Unit = {
    if (selection == null)
      return
    val witems = walker.items.value()
    val cidx = witems.indexOf(selection)
    if (cidx < 0)
      return
    val newIdx = cidx + offset
    if (0 <= newIdx && newIdx < witems.length )
      walker.select(witems(newIdx))
  }



  /** Creates vertical component list. */
  private def mkUI(sorter : JComponent, filter : JComponent, body : JComponent) : JComponent = {
    val res = new JPanel()
    res.setLayout(new GridBagLayout())

    val gbc = new GridBagConstraints()
    gbc.weightx = 1
    gbc.weighty = 0
    gbc.gridx = 0
    gbc.gridy = 0
    gbc.fill = GridBagConstraints.HORIZONTAL
    gbc.anchor = GridBagConstraints.NORTH

    res.add(sorter, gbc)
    gbc.gridy += 1
    res.add(filter, gbc)
    gbc.gridy += 1
    gbc.weighty = 1
    gbc.fill = GridBagConstraints.BOTH
    res.add(body, gbc)

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
  def navActionsFor(prefix : String, couldNav : Behaviour[Boolean], w : FileWalker) : Seq[ActionSpec] =
    Seq(
      prefix + "open" :-> {
        if (couldNav.value)
          w.open
      },
      prefix + "up" :-> w.openItem(FileInfo.ParentDirectory),
      prefix + "crossnav" :-> w.crossNavNext(),
      prefix + "siblingnav" :-> w.siblingNavNext()
    )


  /**
   * Creates default key bindings for actions with specific prefix.
   */
  def defaultKeyBindings(prefix : String) : Seq[(String, String)] =
    DEFAULT_ACTION_MAP.map(x ⇒ (x._1, prefix + x._2))
}
