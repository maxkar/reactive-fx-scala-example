package ru.maxkar.reactive.value

import ru.maxkar.reactive.wave._
import ru.maxkar.reactive.iter._


/**
 * Function application behaviour.
 */
private[value] final class FmapBehaviour[S, T](
      fn : S ⇒ T, base : Behaviour[S])
    extends Behaviour[T] {



  /** Value of this behaviour. */
  private var v = fn(base.value)



  /** Key and pin of this node. */
  private val (key, pin1) =
    Wave.pin(false, singleIter(base.pin), update)
  base.pin.addDependency(key)



  /** Updates this behaviour. */
  private def update() : Boolean = {
    if (!base.pin.value)
      return false
    val nv = fn(base.value)
    if (nv == v)
      return false
    v = nv
    return true
  }



  def value() : T = v
  override val pin = pin1
}
