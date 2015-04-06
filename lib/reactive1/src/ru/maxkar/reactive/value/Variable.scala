package ru.maxkar.reactive.value

import ru.maxkar.reactive.proc._

/**
 * Mutable/changeable behaviour. Could be explicitly set by user.
 * @param T variable type.
 * @param v initial and current value.
 */
final class Variable[T] private[value](private var v : T)
    extends Behaviour[T] {

  /** Next value to set. */
  private var nextValue : Option[T] = None


  /** "Changed" flag for this node. */
  private var changed = false


  /** Action and procedure for this node. */
  private val (action, proc) =
    Procedure.generator(updateValue, () ⇒ changed = false)


  /** Updates actual value. */
  private def updateValue() : Unit = {
    if (nextValue.get != v) {
      changed = true
      v = nextValue.get
    }
    nextValue = None
  }


  /* IMPLEMENTATION */
  override def value() : T = v
  override val change = Signal(proc, changed)


  /** Sets a new value. */
  def set(newValue : T) : Unit =
    nextValue match {
      case None ⇒
        if (newValue != v) {
          nextValue = Some(newValue)
          action.activate()
        }
      case Some(x) ⇒
        nextValue = Some(newValue)
    }



  /** Returns this variable as behaviour. Could be useful for functors, etc... */
  val behaviour : Behaviour[T] = this
}
