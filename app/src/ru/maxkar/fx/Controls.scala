package ru.maxkar.fx

import java.awt.event.ActionListener
import java.awt.event.ActionEvent

import javax.swing.JComboBox
import javax.swing.JComponent

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
}
