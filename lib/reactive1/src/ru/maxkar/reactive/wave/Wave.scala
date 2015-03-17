package ru.maxkar.reactive.wave


import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

/**
 * One update wave. Tracks and processes pins.
 */
final class Wave {
  /** Nodes to "boot" (prepare initial deps and calls). */
  private def bootQueue = new Queue[Pin[_]]



  /** Number of "loaded" items. */
  private var bootCount = 0


  /**
   * Queue used to put resolved nodes.
   */
  private def resolveQueue = new Queue[Pin[_]]



  /**
   * Queue used to clean up nodes.
   */
  private def cleanupQueue = new Queue[Pin[_]]



  /**
   * Processes nodes starting from boot ones.
   */
  private def process(boot : Seq[Pin[_]]) : Unit = {
    val prebootQueue = new Queue[Pin[_]]

    /** Process deep prebuilt dependencies. */
    preboot(prebootQueue, boot)
    while (!prebootQueue.isEmpty)
      preboot(prebootQueue, prebootQueue.dequeue.getDeps())


    /* Processing loop. All new changes will form a new wave,
     * only new nodes could be added and will end up in the boot queue.
     */
    do {
      while (!bootQueue.isEmpty) {
        val item = bootQueue.dequeue
        if (item.bootAndUpdate())
          resolveQueue += item
      }

      while (!resolveQueue.isEmpty) {
        val rtDepIterator = resolveQueue.dequeue.resolve().iterator
        while (rtDepIterator.hasNext) {
          val item = rtDepIterator.next
          if (item.rollAndUpdate())
            resolveQueue += item
        }
      }

      /* while (!bootQueue.isEmpty || !resolveQueue.isEmpty) but
       * resolveQueue is empty due to a previous condition.
       * Boot queue could contain new nodes created by actions so it is
       * not safe to omit it.
       */
    } while (!bootQueue.isEmpty)



    /* Reset state for all involved nodes. */
    if (cleanupQueue.size != bootCount)
      throw new AssertionError(
        "Cyclic dependencies found, have to proces " + bootCount +
        " nodes but processed only " + cleanupQueue.size)

    val itr = cleanupQueue.iterator
    while (itr.hasNext)
      itr.next.cleanup()
  }



  /**
   * Inserts a new node into the wave propagation. This
   * new node should not have any dependencies and is added
   * with the goal of deferring/ordering calculation of other
   * nodes during the wave.
   */
  private def forceAdd(pin : Pin[_]) : Unit = {
    if (!pin.activate())
      throw new IllegalStateException("Bad pin to force add")
    bootCount += 1
    bootQueue += pin
  }



  /**
   * Preboots list of nodes and adds all "activated" nodes into the queue.
   */
  private def preboot(queue : Queue[Pin[_]], boot : Iterable[Pin[_]]) : Unit = {
    val iiter = boot.iterator
    while (iiter.hasNext) {
      val item = iiter.next
      if (item.activate()) {
        bootCount += 1
        bootQueue += item
        queue += item
      }
    }
  }
}




/**
 * Wave initiation and processing object.
 */
object Wave {
  /**
   * Wave processing lock.
   */
  private val lock = new Object()



  /** Enqueued actions. */
  private var actionQueue : ArrayBuffer[Action[_, _, _]] = null



  /** Current wave. */
  private var wave : Wave = null



  /**
   * Registers an action. If no update/actions are in progress,
   * executes action immediately.
   */
  private[wave] def addAction(action : Action[_, _, _]) : Unit =
    actionQueue += action



  /**
   * Executes a "batch" process where all actions are updated
   * and wave propagates across all dependent nodes.
   */
  def batch(proc : ⇒ Unit) : Unit =
    lock synchronized {
      if (actionQueue != null) {
        proc
        return
      }

      actionQueue = new ArrayBuffer[Action[_, _, _]]()
      try {
        proc

        while (!actionQueue.isEmpty) {
          val cur = actionQueue
          actionQueue = new ArrayBuffer[Action[_, _, _]]
          wave = new Wave()
          wave.process(cur.map(x ⇒ x.prepare()))
        }
      } finally {
        wave = null
        actionQueue = null
      }
    }
}
