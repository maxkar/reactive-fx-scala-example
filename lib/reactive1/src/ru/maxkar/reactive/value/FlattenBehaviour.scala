package ru.maxkar.reactive.value

import ru.maxkar.reactive.iter._
import ru.maxkar.reactive.wave._


/** Flattening behaviour. */
private[value] final class FlattenBehaviour[T](
      base : Behaviour[Behaviour[T]])
    extends Behaviour[T] {

  private var vv = base.value
  private var v = vv.value
  private val (key, pin1) =
    Wave.pin(false,
      seqIters(
        singleIter(base.pin),
        () ⇒ depSingle(base.value.pin)),
      update)
  vv.pin.addDependency(key)
  base.pin.addDependency(key)



  override def value() = v
  override val pin = pin1



  /** Updates this node. */
  private def update() : Boolean = {
    if (!base.pin.value && !base.value.pin.value)
      return false

    if (base.pin.value) {
      vv.pin.removeDependency(key)
      vv = base.value
      vv.pin.addDependency(key)
    }


    val nv = vv.value
    if (nv == v)
      return false

    v = nv
    return true
  }
}
