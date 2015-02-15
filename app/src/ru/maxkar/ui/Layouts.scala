package ru.maxkar.ui

import java.awt.BorderLayout
import java.awt.FlowLayout

import javax.swing.JComponent
import javax.swing.JPanel


/**
 * Common UI layout utilities.
 */
object Layouts {
  /**
   * Creates a layout with one component in center and
   * at most four other components around its borders.
   */
  def border(
        center : JComponent = null,
        north : JComponent = null,
        east : JComponent = null,
        south : JComponent = null,
        west : JComponent = null)
      : JComponent = {
    val res = new JPanel(new BorderLayout)

    if (center != null)
      res.add(BorderLayout.CENTER, center)
    if (north != null)
      res.add(BorderLayout.NORTH, north)
    if (east != null)
      res.add(BorderLayout.EAST, east)
    if (south != null)
      res.add(BorderLayout.SOUTH, south)
    if (west != null)
      res.add(BorderLayout.WEST, west)

    res
  }



  /**
   * Creates simple left-to-right flow layout.
   */
  def leftToRightFlow(components : JComponent*) : JComponent = {
    val res = new JPanel(new FlowLayout(FlowLayout.LEADING))
    components foreach res.add
    res
  }
}
