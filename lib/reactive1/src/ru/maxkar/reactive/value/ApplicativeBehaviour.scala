package ru.maxkar.reactive.value

import ru.maxkar.reactive.wave._
import ru.maxkar.reactive.iter._


/**
 * Function application behaviour.
 */
private[value] final class ApplicativeBehaviour[S, R](
      fn : Behaviour[S ⇒ R], base : Behaviour[S])
    extends Behaviour[R] {

  /** Internal value. */
  private var v = fn.value()(base.value)
  private var (key, pin1) =
    Wave.pin(false, itemsIter(fn.pin, base.pin), update)
  fn.pin.addDependency(key)
  base.pin.addDependency(key)



  override def value() : R = v
  override val pin = pin1


  /**
   * Updates this value.
   */
  private def update() : Boolean = {
    if (!fn.pin.value && !base.pin.value)
      return false

    val nv = fn.value()(base.value)
    if (nv == v)
      return false

    v = nv
    return true
  }
}
