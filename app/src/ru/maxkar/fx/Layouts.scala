package ru.maxkar.fx

import javax.swing.JComponent
import javax.swing.JPanel
import java.awt.GridBagLayout

/**
 * Layout utilities.
 */
object Layouts {

  /**
   * Wraps a node into "float center" box.
   */
  def floatCenter(node : JComponent) : JComponent = {
    val res = new JPanel()
    res setLayout new GridBagLayout()
    res add node
    res
  }
}
