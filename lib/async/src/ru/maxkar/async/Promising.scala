package ru.maxkar.async

/** Trait for objects which can create promises. */
trait Promising {
  /** Launches an async operation and returns a result. */
  def apply[T](item : ⇒ T) : Promise[T]
}
