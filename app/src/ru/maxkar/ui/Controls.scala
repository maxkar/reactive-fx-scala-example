package ru.maxkar.ui

import java.awt.BorderLayout

import java.awt.event.ActionListener
import java.awt.event.ActionEvent

import javax.swing.JComponent
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JPanel

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
    cur ≺ res.setSelectedItem

    res addActionListener new ActionListener() {
      override def actionPerformed(e : ActionEvent) : Unit = {
        val mitem = res.getSelectedItem().asInstanceOf[T]
        if (cur.value == mitem)
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
}
