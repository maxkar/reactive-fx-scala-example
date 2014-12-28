package ru.maxkar.reactive.value.event

import ru.maxkar.reactive.wave.Participable
import ru.maxkar.reactive.wave.Participant
import ru.maxkar.reactive.wave.Wave

/**
 * Simple trigger event source, each trigger can be "triggered"
 * and will change to an armed state to the span of the wave.
 * After the wave this trigger will be reset to initial (false)
 * state.
 */
final class Trigger private[event](ctx : Participable) {

  /** Value of this node. */
  private var fired = false



  /** Flow participant for this node. */
  private val participant = ctx.participant((x) ⇒ (), (x) ⇒ (), cleanup)



  /** Event node for the trigger. */
  val event : Event[Boolean] = new Event[Boolean] {
    override def addCorrelatedNode(node : Participant) : Unit =
      participant.addCorrelatedNode(node)

    override def removeCorrelatedNode(node : Participant) : Unit =
      participant.removeCorrelatedNode(node)

    override def value() : Boolean = fired

    override def defer(peer : Participant) : Unit =
      peer.defer(participant)
  }




  /**
   * Triggers this trigger and changes it to an "armed" state.
   * @param wave wave in which this change will be triggered.
   */
  def trigger(wave : Wave) : Unit = {
    participant.engage(wave)
    fired = true
  }



  /* Cleans up after the wave. */
  private def cleanup() : Unit = fired = false
}

