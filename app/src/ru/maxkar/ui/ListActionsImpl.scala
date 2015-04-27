package ru.maxkar.ui

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import javax.swing.JList


/** List actions handler. */
private[ui] final class ListActionsImpl[T <: AnyRef](
        items : Behaviour[Seq[T]],
        selectedItem : Behaviour[T],
        selector : T ⇒ Unit,
        list : JList[T])
      extends ListActions {



  private def pageUpIdx() : Int = {
    val idx = list.getSelectedIndex()
    if (idx < 0)
      if (items.value.length > 0)
        return 0
      else
        return -1

    val lbnd = list.getFirstVisibleIndex()
    if (lbnd == 0 && idx <= list.getLastVisibleIndex())
      return 0

    val rbnd = list.getLastVisibleIndex()
    return Math.max(0, idx - (rbnd - lbnd - 1))
  }



  private def pageDnIdx() : Int = {
    val idx = list.getSelectedIndex()
    if (idx < 0)
      if (items.value.length > 0)
        return 0
      else
        return -1

    val lim = items.value.length - 1
    val rbnd = list.getLastVisibleIndex()
    if (rbnd == lim && idx >= list.getFirstVisibleIndex())
      return lim

    val lbnd = list.getFirstVisibleIndex()
    return Math.min(lim, idx + (rbnd - lbnd - 1))
  }



  private def pageNav(idx : Int) : Unit = {
    if (idx < 0)
      selector(null.asInstanceOf[T])
    else
      selector(items.value()(idx))
  }



  private def relNav(offset : Int) : Unit = {
    val idx = list.getSelectedIndex()
    if (idx < 0) {
      if (items.value.length > 0)
        selector(items.value().head)
      return
    }

    var nidx = idx + offset
    if (nidx < 0)
      nidx = 0
    if (nidx >= items.value.length)
      nidx = items.value.length - 1

    if (nidx != idx)
      selector(items.value()(nidx))
  }


  override def pageDown() : Unit = pageNav(pageDnIdx())


  override def pageUp() : Unit = pageNav(pageUpIdx())


  override def nextItem() : Unit = relNav(1)


  override def prevItem() : Unit = relNav(-1)
}
