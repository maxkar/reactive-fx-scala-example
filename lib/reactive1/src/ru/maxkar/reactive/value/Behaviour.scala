package ru.maxkar.reactive.value


/**
 * Reactive value which could change over time.
 */
trait Behaviour[T] {
  /** Value change signal. */
  val change : Signal[Boolean]

  /** Retrieves value of this behaviour. */
  def value() : T
}
