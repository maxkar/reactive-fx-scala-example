package ru.maxkar.fx

import javafx.scene.Node
import javafx.scene.image._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/** Image utilities. */
object Images {
  /** Creates a simple image view for the image behaviour. */
  def simpleView(
        state : Behaviour[Image])(
        implicit lifespan : Lifespan)
      : Node = {
    val res = new ImageView
    res.setImage _ :> state
    res
  }
}

