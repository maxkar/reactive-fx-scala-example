package ru.maxkar.fx

import javafx.scene._
import javafx.scene.layout._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/** Javafx node operations. */
object Nodes {

  /** Creates a node which have a content of the target node. */
  def contentOf(
        ui : Behaviour[Node])(
        implicit lifespan : Lifespan)
      : Node = {

    val res = new BorderPane()
    res.setCenter _ :> ui

    res
  }
}
