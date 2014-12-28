package ru.maxkar.reactive.value

import ru.maxkar.reactive.value.event.Event
import ru.maxkar.reactive.wave.Participant
import ru.maxkar.reactive.wave.Wave


/**
 * Behaviour which applies a "map" function to get another reactive value.
 * @param S source type.
 * @param T destination (value) type.
 * @param mapper map function.
 * @param source source behaviour.
 */
private[value] final class MapBehaviour[S, T](
      mapper : S ⇒ T, source : Behaviour[S],
      ctx : BindContext)
    extends Behaviour[T] {

  /** Wave participant. */
  private val participant =
    ctx.update.participant(participate, resolved, reset)
  source.change.addCorrelatedNode(participant)



  /** Current value. */
  private var currentValue = mapper(source.value)



  /** Flag indicating that value was changed during current wave. */
  private var changed = false



  /** Participation handler. */
  private def participate(w : Wave) : Unit =
    source.change.defer(participant)



  /** Marks this node as resovled. */
  private def resolved(w : Wave) : Unit = {
    /* No update, just return. */
    if (!source.change.value)
      return

    val newValue = mapper(source.value)
    if (newValue == currentValue)
      return

    currentValue = newValue
    changed = true
  }



  /** Resets this node after wave completion. */
  private def reset() : Unit = changed = false



  /** Disposes this node. */
  private[value] def dispose() : Unit =
    source.change.removeCorrelatedNode(participant)



  /* IMPLEMENTATION. */

  override def value() : T = currentValue

  override val change = Event.fromParticipant(participant, changed)
}

