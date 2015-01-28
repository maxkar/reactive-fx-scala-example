package ru.maxkar.fx


import javax.swing.JComponent
import javax.swing.JButton

import java.awt.event.ActionEvent
import java.awt.event.ActionListener


/** Button factories. */
object Buttons {
  def simplestButton(title : String, action : ⇒ Unit) : JComponent = {
    val res = new JButton(title)
    res addActionListener new ActionListener {
      override def actionPerformed(e : ActionEvent) : Unit =
        action
    }
    res
  }
}
