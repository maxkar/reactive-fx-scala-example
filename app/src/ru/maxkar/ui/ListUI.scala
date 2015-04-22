package ru.maxkar.ui

import java.awt.event._

import javax.swing.JComponent
import javax.swing.JList
import javax.swing.ListCellRenderer

import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


import scala.reflect.ClassTag



/** List UI factory. */
private[ui] final object ListUI {
  /**
   * Creates a list component.
   * @param items list items.
   * @param selectedItem selection model.
   * @param selector element to use when list item is selected.
   * @param renderer list item renderer.
   * @return Dynamic list ui.
   */
  def list[T <: AnyRef : ClassTag](
        items : Behaviour[Seq[T]],
        selectedItem : Behaviour[T],
        selector : T ⇒ Unit,
        renderer : ListCellRenderer[T])(
        implicit ctx : BindContext)
      : JComponent = {
    val list = new JList[T](items.value.toArray)

    var updating = false

    list setCellRenderer renderer

    def syncModels(nitems : Seq[T], sel : T) : Unit = {
      updating = true
      try {
        val lc = items.change.value
        val sc = selectedItem.change.value

        if (lc)
          list.setListData(nitems.toArray)

        if (lc || sc)
          list.setSelectedValue(sel, true)

        val si = list.getSelectedIndex()
        if (si > 0)
          list.ensureIndexIsVisible(si - 1)
        if (si < nitems.length - 1)
          list.ensureIndexIsVisible(si + 1)
        list.ensureIndexIsVisible(si)
      }  finally {
        updating = false
      }
    }



    def pageUpIdx() : Int = {
      val idx = list.getSelectedIndex()
      if (idx < 0)
        return -1

      val lbnd = list.getFirstVisibleIndex()
      if (lbnd == 0 && idx <= list.getLastVisibleIndex())
        return 0

      val rbnd = list.getLastVisibleIndex()
      return Math.max(0, idx - (rbnd - lbnd - 1))
    }



    def pageDnIdx() : Int = {
      val idx = list.getSelectedIndex()
      if (idx < 0)
        return -1


      val lim = items.value.length - 1
      val rbnd = list.getLastVisibleIndex()
      if (rbnd == lim && idx >= list.getFirstVisibleIndex())
        return lim

      val lbnd = list.getFirstVisibleIndex()
      return Math.min(lim, idx + (rbnd - lbnd - 1))
    }



    def pageNav(idx : Int) : Unit =
      selector(items.value()(idx))



    (syncModels _).curried ≻ items ≻ selectedItem
    if (selectedItem.value != null)
      list.setSelectedValue(selectedItem.value, true)


    list addListSelectionListener new ListSelectionListener() {
      override def valueChanged(e : ListSelectionEvent) : Unit = {
        if (updating)
          return
        val idx = list.getSelectedIndex()
        selector(
          if (idx < 0)
            null.asInstanceOf[T]
          else
            items.value()(idx))
      }
    }


    list addKeyListener new KeyAdapter() {
      override def keyPressed(e : KeyEvent) {
        if (e.getModifiers == 0)
          e.getKeyCode match {
            case KeyEvent.VK_PAGE_DOWN ⇒
              pageNav(pageDnIdx())
              e.consume()
            case KeyEvent.VK_PAGE_UP ⇒
              pageNav(pageUpIdx())
              e.consume()
            case _ ⇒ ()
          }
      }
    }

    list
  }
}
