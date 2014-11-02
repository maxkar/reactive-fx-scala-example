package ru.maxkar.fx

import javafx.scene._
import javafx.scene.text._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

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
      : Node = {
    val res = new Text()
    res.setText _ :> value
    res
  }
}
