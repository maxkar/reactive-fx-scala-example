package ru.maxkar.reactive.value

import ru.maxkar.reactive.value.event.Event

/**
 * Common trait for all behaviours. Each behaviour is
 * just a "state" at the current moment of the time. Behaviour
 * is somewhat similar to events. However where event have some
 * "neutral" state, behaviour do not have such state but have
 * some "current" state instead. That "current" state is not reset
 * after each wave.
 * <p>Behaviour users are interested in state changes. However,
 * flow/event API does not have this definition by itself. And
 * handling equality if behaviour deps can be bothersome, each
 * behaviour have a corresponding "change" event source. This
 * event is set to "true" on each wave where value of this
 * behaviour changes.
 * @param T type of the behaviour's value.
 */
trait Behaviour[+T] {
  /**
   * Change event source. This event is activated when value
   * of this behaviour is going to change during the wave.
   * <p> This event is generic change event. Concrete behaviours
   * may define additional event types. For example, list behaviour may
   * define a "list change" event which will be a more fine-grained
   * change description (it may include changed indices).
   */
  val change : Event[Boolean]



  /**
   * Returns current value of this behaviour.
   * During the propagation value is guaranteed to be updated
   * only after the corresponding event is fired/resolved.
   * @return current value of this behaviour.
   */
  def value() : T
}

