package ru.maxkar.reactive.value

import ru.maxkar.reactive.wave._

/**
 * Behaviour variable (behaviour which could be set).
 */
final class Variable[T](private var v : T)  extends Behaviour[T] {
  /** Pin and actor for this variable. */
  private val (pin1, actor) =
    Wave.action[T, T, Boolean](
      false, x ⇒ x, (_, x) ⇒ x, update)


  /** Sets a new behaviour value. */
  def set(nv : T) : Unit = actor(nv)


  override def value() : T = v
  override val pin = pin1


  /** Updates internal value. */
  private def update(nv : T) : Boolean = {
    val changed = nv != v
    if (changed)
      v = nv
    changed
  }
}
