package ru.maxkar.reactive.value

import ru.maxkar.reactive.value.event.Event
import ru.maxkar.reactive.wave.Wave
import ru.maxkar.reactive.wave.Participable

/**
 * Behaviour "variable". This class allows direct "push" request
 * to current state. However, this behaviour never can depend on
 * other behaviours. In the dependency graph this behaviour represents
 * a leaf node.
 * @param T type of the value.
 * @param value currentValue (and initial) value.
 */
final class Variable[T] private[value] (
      private var currentValue : T) {

  /** Event trigger associated with this variable. */
  private val trigger = Event.trigger(Participable.DefaultParticipable)



  /**
   * Sets value as a part of the wave. If new value equals to old value,
   * then new value is ignored.
   * @param newValue new value to set.
   * @param wave current propagation wave.
   */
  def wavedSet(newValue : T, wave : Wave) : Unit = {
    if (currentValue == newValue)
      return
    currentValue = newValue
    trigger.trigger(wave)
  }



  /**
   * Sets a new value in a brand new wave. If new value equals to
   * old value, then new value is ignored and event is not fired.
   * @param newValue new value to set.
   */
  def set(newValue : T) : Unit =
    Wave.group(wave ⇒ wavedSet(newValue, wave))



  /**
   * Behaviour associated with this variable.
   */
  val behaviour : Behaviour[T] = new Behaviour[T] {
    /* IMPLEMENTATION. */
    override def value() : T = currentValue
    override val change : Event[Boolean] = trigger.event
  }
}

