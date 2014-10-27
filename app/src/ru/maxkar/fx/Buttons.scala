package ru.maxkar.fx

import javafx.event.ActionEvent
import javafx.scene._
import javafx.scene.control._

import Bridge._

/** Button factories. */
object Buttons {
  def simplestButton(title : String, action : ⇒ Unit) : Node = {
    val res = new Button(title)
    res setOnAction evt{action}
    res
  }
}
