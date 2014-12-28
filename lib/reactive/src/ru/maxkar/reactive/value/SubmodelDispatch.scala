package ru.maxkar.reactive.value

import ru.maxkar.reactive.value.event.Event
import ru.maxkar.reactive.wave.Participant
import ru.maxkar.reactive.wave.Wave


/**
 * Submodel bound to a lifetime of parent value.
 */
private[value] final class SubmodelDispatch[S, R](
      mapper : (S, BindContext) ⇒ R,
      base : Behaviour[S],
      ctx : BindContext)
    extends Behaviour[R] {

  /** Wave participant. */
  private val participant =
    ctx.update.participant(participate, resolved, reset)
  base.change.addCorrelatedNode(participant)



  /**
   * Child lifecycle. Set to <code>null</code> after this
   * node is disposed.
   */
  private var childLifecycle = new Session()



  /**
   * Nested (inner) model, produced by the function for
   * the current value).
   */
  private var currentValue =
    mapper(base.value,
      new BindContext(childLifecycle, ctx.update))



  /** Flag indicating that value was changed during current wave.  */
  private var changed = false



  /** Participation handler. */
  private def participate(w : Wave) : Unit =
    base.change.defer(participant)



  /** Marks this node as resovled. */
  private def resolved(w : Wave) : Unit = {
    if (childLifecycle == null)
      return

    if (!base.change.value)
      return


    childLifecycle.destroy()
    childLifecycle = new Session()
    val newValue = mapper(base.value, new BindContext(childLifecycle, w))
    if (newValue != currentValue) {
      changed = true
      currentValue = newValue
    }
  }



  /** Resets this node after wave completion. */
  private def reset() : Unit = changed = false



  /** Disposes this node. */
  private[value] def dispose() : Unit = {
    base.change.removeCorrelatedNode(participant)
    childLifecycle.destroy()
    childLifecycle = null
  }



  /* IMPLEMENTATION. */

  override def value() : R = currentValue

  override val change = Event.fromParticipant(participant, changed)
}

