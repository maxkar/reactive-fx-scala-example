package ru.maxkar.reactive.value

import ru.maxkar.reactive.value.event.Event
import ru.maxkar.reactive.wave.Participant
import ru.maxkar.reactive.wave.Wave

/**
 * Applicative function application implementation.
 * @palam fn applicative function.
 * @param base base value.
 */
private[value] final class ApplicativeBehaviour[S, R](
      fn : Behaviour[S ⇒ R],
      base : Behaviour[S],
      context : BindContext)
    extends Behaviour[R] {


  /** Wave participant. */
  private val participant =
    context.update.participant(participate, resolved, reset)
  fn.change.addCorrelatedNode(participant)
  base.change.addCorrelatedNode(participant)



  /** Current value. */
  private var currentValue = fn.value()(base.value)



  /** Flag indicating that value was changed during current wave. */
  private var changed = false



  /** Participation handler. */
  private def participate(w : Wave) : Unit = {
    fn.change.defer(participant)
    base.change.defer(participant)
  }



  /** Marks this node as resovled. */
  private def resolved(w : Wave) : Unit = {
    /* No update, just return. */
    if (!fn.change.value && !base.change.value)
      return

    val newValue = fn.value()(base.value)
    if (newValue == currentValue)
      return

    currentValue = newValue
    changed = true
  }



  /** Resets this node after wave completion. */
  private def reset() : Unit = changed = false



  private[value] def dispose() : Unit = {
    fn.change.removeCorrelatedNode(participant)
    base.change.removeCorrelatedNode(participant)
  }



  /* IMPLEMENTATION. */

  override def value() : R = currentValue

  override val change = Event.fromParticipant(participant, changed)
}

