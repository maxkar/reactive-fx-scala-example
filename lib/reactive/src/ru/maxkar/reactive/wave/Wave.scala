package ru.maxkar.reactive.wave

import scala.collection.mutable.Queue

/**
 * One propagation (update) wave. Each wave is one "transaction"
 * of the events. Each node involved in the transaction fire
 * exactly one event which can be consumed by all the registered
 * listeners.
 */
final class Wave private() extends Participable {

  /** Current propagation state. */
  private var stage = Wave.STAGE_NEW



  /**
   * Wave queue. These participants will be "engaged" during the
   * engage phase and will have a goal to establish all nodes reacheable
   * by this wave. Wave may include extra nodes (for example, nodes which
   * do not produce event for this wave), but would not include nodes not
   * reacheable from the engaged nodes.
   */
  private val engageQueue = new Queue[Participant]



  /**
   * Item boot queue. Holds nodes to perform first resolution
   * attempt.
   */
  private val bootQueue = new Queue[Participant]



  /**
   * Queue of nodes ready to resolve. These nodes have all dependencies
   * satisfied but "resolution listeners" not fired. This helps to keep
   * stack depth manageable (no recursive calls from listeners to listeners).
   */
  private val resolveQueue = new Queue[Participant]



  override def participant(
        onBoot : Wave ⇒ Unit,
        onResolved : Wave ⇒ Unit,
        onCleanup : () ⇒ Unit)
      : Participant = {
    if (stage != Wave.STAGE_RESOLUTION)
      throw new IllegalStateException(
        "Attempt to create participant in an unexpected state of " + stage)
    val res = new Participant(onBoot, onResolved, onCleanup)
    res.innerEngage()
    bootQueue += res
    res
  }


  /**
   * Adds a listener which will be invoked during the engage state.
   * <strong>This method should be invoked from the participant only.</strong>
   * @param participant participanT to join this wave.
   * @throws IllegalStateException if this wave has passed engagement phase.
   */
  private[wave] def enqueueParticipant(participant : Participant) : Unit = {
    if (stage > Wave.STAGE_ENGAGEMENT)
      throw new IllegalStateException(
        "Too late to engage in this wave at phase " + stage)
    engageQueue += participant
  }



  /**
   * Enqueues a node into the resolve queue. This mean that node
   * wants to attempt to resolve it. During the resolution process
   * node may discover additional dependencies and still remain
   * in "undecided" state.
   * @param node node to add.
   * @throws IllegalStateException if this wave is not in resolution state.
   */
  private[wave] def enqueueResolved(node : Participant) : Unit = {
    if (stage < Wave.STAGE_ENGAGEMENT || stage > Wave.STAGE_RESOLUTION)
      throw new IllegalStateException(
        "Bad state for resolution attemts, stage = " + stage)
    resolveQueue += node
  }



  /**
   * Runs a wave.
   * @throws Error if some nodes were unable to resolve.
   */
  private[wave] def run() : Unit = {
    val cleanupQueue = new Queue[Participant]

    stage = Wave.STAGE_ENGAGEMENT
    while (!engageQueue.isEmpty) {
      val node = engageQueue.dequeue
      bootQueue += node
      node.engageComplete(this)
    }


    var involvedNodes = 0
    stage = Wave.STAGE_RESOLUTION

    do {
      while (!bootQueue.isEmpty) {
        bootQueue.dequeue.boot(this)
        involvedNodes += 1
      }
      while (!resolveQueue.isEmpty) {
        val node = resolveQueue.dequeue
        node.notifyDeps(this)
        cleanupQueue += node
      }
      /* Same as while (... && !resoveQueue.isEmpty) */
    } while (!bootQueue.isEmpty)


    if (involvedNodes != cleanupQueue.size)
      throw new Error(
        "Node completion mismatch, expected " + involvedNodes +
        " but got " + cleanupQueue.size)

    stage = Wave.STAGE_CLEANUP
    while (!cleanupQueue.isEmpty)
      cleanupQueue.dequeue.cleanup()
  }
}


/**
 * Wave companion object.
 */
final object Wave {
  /** Wave is new and not started. */
  private val STAGE_NEW = 0

  /** Wave is processing engagements. */
  private val STAGE_ENGAGEMENT = 1

  /** Resolution phase. */
  private val STAGE_RESOLUTION = 2

  /** Cleanup phase. */
  private val STAGE_CLEANUP = 3

  /** Final death. */
  private val STAGE_DEAD = 4



  /**
   * Runs evaluation as part of the change "group" for the new wave.
   * Wave is resolved before returning from this method.
   * @param block block to execute on a new wave.
   */
  def group(block : Wave ⇒ Unit) : Unit = {
    val wave = new Wave()
    block(wave)
    wave.run()
  }
}

