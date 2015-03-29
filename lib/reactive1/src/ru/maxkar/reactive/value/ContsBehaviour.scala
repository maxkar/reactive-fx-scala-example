package ru.maxkar.reactive.value

import ru.maxkar.reactive.wave._
import ru.maxkar.reactive.iter._


/**
 * Constant (never changing) behaviour.
 */
private[value] final class ConstBehaviour[T](val v : T)
      extends Behaviour[T] {
  override def value() : T = v
  val pin = Wave.pin[Boolean](false, emptyIter, () ⇒ false)._2
}
