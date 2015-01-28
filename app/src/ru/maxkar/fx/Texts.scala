package ru.maxkar.fx


import javax.swing.JComponent
import javax.swing.JTextArea

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/**
 * Text bindings and operations.
 */
object Texts {
  /**
   * Creates a simple text display.
   */
  def simpleText(
        value : Behaviour[String])(
        implicit ctx : BindContext)
      : JComponent = {
    val res = new JTextArea()
    value ≺ res.setText
    res setEditable false
    res
  }
}
