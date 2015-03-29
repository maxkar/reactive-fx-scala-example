package ru.maxkar.reactive.value

import ru.maxkar.reactive.wave._

/**
 * Common trait for all value. Each behaviour is
 * @param T type of the behaviour's value.
 */
trait Behaviour[+T] {
  /**
   * Returns current value of this behaviour.
   * During the propagation value is guaranteed to be updated
   * only after the corresponding event is fired/resolved.
   * @return current value of this behaviour.
   */
  def value() : T



  /** Behaviour pin. */
  val pin : Pin[Boolean]
}
