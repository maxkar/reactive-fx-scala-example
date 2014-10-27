package ru.maxkar.fx

import javafx.scene._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/** Javafx node operations. */
object Nodes {

  /** Creates a node which have a content of the target node. */
  def contentOf(
        ui : Behaviour[Node])(
        implicit lifespan : Lifespan)
      : Node = {

    val res = new Group()
    var last : Node = null

    def update(node : Node) : Unit = {
      if (last != null)
        res.getChildren() remove last
      last = node
      if (node != null)
        res.getChildren() add node
    }

    update _ :> ui

    res
  }
}
