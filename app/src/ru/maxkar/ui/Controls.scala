package ru.maxkar.ui

import java.awt.BorderLayout

import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.Graphics
import java.awt.Color

import javax.swing.JComponent
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JSplitPane
import javax.swing.JTextField
import javax.swing.ListCellRenderer

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import scala.reflect.ClassTag


/**
 * Simple controls factory.
 */
object Controls {
  /**
   * Creates a combobox control for the static list.
   */
  def combo[T <: AnyRef : ClassTag](
        items : Seq[T], cur : Behaviour[T], selector : T ⇒ Unit)(
        implicit ctx : BindContext)
      : JComponent = {
    val res = new JComboBox[T](items.toArray[T])
    cur ≺ (x ⇒
      res.setSelectedItem(if (items.contains(x)) x else null))

    res addActionListener new ActionListener() {
      override def actionPerformed(e : ActionEvent) : Unit = {
        val mitem = res.getSelectedItem().asInstanceOf[T]
        if (mitem == null || cur.value == mitem)
          return
        selector(mitem)
        /* Rebind value because model is authoritative, not a combo. */
        if (cur.value != mitem)
          res.setSelectedItem(cur.value)
      }
    }

    res
  }



  /**
   * Creates a general-purpose text display component (label).
   */
  def label(
        value : Behaviour[String])(
        implicit ctx : BindContext)
      : JComponent = {
    val res = new JLabel()
    value ≺ res.setText
    res
  }



  /** Creates a node which have a content of the target node. */
  def contentOf(
        ui : Behaviour[JComponent])(
        implicit ctx : BindContext)
      : JComponent = {
    var last : JComponent = null

    val res = new JPanel()
    res setLayout new BorderLayout()

    def update(nc : JComponent) : Unit = {
      if (last != null)
        res.remove(last)
      last = nc
      if (nc != null)
        res.add(nc, BorderLayout.CENTER)
      res.revalidate()
      res.repaint()
    }
    ui ≺ update

    res
  }



  /**
   * Creates a button by its name and action handler.
   */
  def button(title : String, action : ⇒ Unit) : JComponent = {
    val res = new JButton(title)
    res addActionListener new ActionListener {
      override def actionPerformed(e : ActionEvent) : Unit =
        action
    }
    res
  }



  /**
   * Creates a horizontal split pane.
   */
  def hsplit(
        left : JComponent,
        right : JComponent,
        leftGrowWeight : Double = 0.0)
      : JComponent = {
    val res = new JSplitPane()
    res setLeftComponent left
    res setRightComponent right
    res setResizeWeight leftGrowWeight
    res
  }



  /**
   * Creates a "lock UI" pane. This pane is intended to be used as
   * glass pane or top level pane in layered pane. This pane locks both
   * focus and mouse interaction.
   */
  def lockPane(
        locked : Behaviour[Boolean])(
        implicit ctx : BindContext)
      : JComponent =
    LockPane.create(locked)



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
      : (JComponent, ListActions) =
    ListUI.list(items, selectedItem, selector, renderer)



  /**
   * Creates a new text input using behaviour and change callback.
   */
  def input(
        value : Behaviour[String],
        cb : String ⇒ Unit)(
        implicit ctx : BindContext)
      : JTextField =
    InputUI.input(value, cb)
}
