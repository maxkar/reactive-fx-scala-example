package ru.maxkar.fx

import javafx.geometry._
import javafx.scene.Node
import javafx.scene.layout._

/**
 * Layout utilities.
 */
object Layouts {

  /**
   * Wraps a node into "float center" box.
   */
  def floatCenter(node : Node) : Region = {
    val gb = new GridPane()
    gb.add(node, 0, 0)
    GridPane.setHalignment(node, HPos.CENTER)
    GridPane.setValignment(node, VPos.CENTER)
    GridPane.setHgrow(node, Priority.ALWAYS)
    GridPane.setVgrow(node, Priority.ALWAYS)
    gb
  }
}
