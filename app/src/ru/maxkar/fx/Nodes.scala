package ru.maxkar.fx

import javafx.scene._
import javafx.scene.layout._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/** Javafx node operations. */
object Nodes {

  /** Creates a node which have a content of the target node. */
  def contentOf(
        ui : Behaviour[Node])(
        implicit ctx : BindContext)
      : Node = {

    val res = new BorderPane()
    res.setCenter _ ≻ ui

    res
  }



  /** Creates a region which have a content of the target node. */
  def regionOf(
        ui : Behaviour[Node])(
        implicit ctx : BindContext)
      : Region = {

    val res = new BorderPane()
    res.setCenter _ ≻ ui

    res
  }
}
