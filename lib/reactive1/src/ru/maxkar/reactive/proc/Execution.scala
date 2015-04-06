package ru.maxkar.reactive.proc


/** One process execution (one iteration).  */
private[proc] final class Execution {

  /** Number of started/initialized items. */
  private var bootItems = 0

  /** Activation queue. */
  private val activationQueue = new ProcList()

  /** Process resolution queue. */
  private val resolvQueue = new ProcList()

  /** Process cleanup queue. */
  private val cleanupQueue = new ProcList()


  /** Adds a procedure into the activation queue. */
  private[proc] def += (p : Procedure) : Unit =
    if (p.activate()) {
      activationQueue += p
      bootItems += 1
    }



  /** Adds process into the resolve queue instead of boot queue. */
  private[proc] def addImmediate(p : Procedure): Unit = {
    p.activate()
    resolvQueue += p
    bootItems += 1
  }



  /** Processes queue activation by taking items from
   * activation queue and processing their dependencies.
   */
  private def processActivation() : Unit =
    while (!activationQueue.empty) {
      val cur = activationQueue.take
      val itr = cur.activations.iterator
      while (itr.hasNext)
        this += itr.next
      resolvQueue += cur
    }



  /** Resolves items. */
  private def resolve() : Unit =
    while (!resolvQueue.empty) {
      val item = resolvQueue.take
      var next = item.roll()
      while (next != null && !next.isActive())
        next = item.roll()

      if (next != null)
        next.deps += item
      else {
        cleanupQueue += item
        resolvQueue.fillFrom(item.deps)
      }
    }


  /** Cleans this execution. */
  private def cleanup() : Int = {
    var resolvCount = 0
    while (!cleanupQueue.empty) {
      cleanupQueue.take.cleanup()
      resolvCount += 1
    }
    resolvCount
  }



  /** Runs an execution. */
  private[proc] def run() : Unit = {
    processActivation()
    resolve()
    val cleanCount = cleanup()
    if (cleanCount != bootItems)
      throw new IllegalStateException("Cleaned " + cleanCount + " items out of " + bootItems)
  }
}




/**
 * Procedure manipulation object.
 */
private[proc] object Execution {
  /** Last activator in the queue. Dummy value. */
  private val lastActivator = new Activator(null)


  /** Activation queue for current or next run. */
  private var activatorQueue : Activator = null


  /** Active execution. */
  private var execution : Execution = null


  /** Synchronization lock. */
  private val lock = new Object



  /**
   * Performs a batch operation. All activated procedures will be executed as
   * one propagation/update wave.
   */
  def batch(cb : ⇒ Unit) : Unit =
    lock synchronized {
      if (activatorQueue != null)
        cb
      else {
        activatorQueue = lastActivator
        cb
        runExecutions
      }
    }



  /** Activates an activator. */
  def activate(activator : Activator) : Unit = {
    if (activator.next != null)
      return

    lock synchronized {
      if (activatorQueue != null) {
        activator.next = activatorQueue
        activatorQueue = activator
      } else {
        activator.next = lastActivator
        activatorQueue = activator
        runExecutions()
      }
    }
  }


  /**
   * Executes update waves.
   */
  private def runExecutions() : Unit = {
    while (activatorQueue.next != null)
      runOneExecution()
    activatorQueue = null
  }



  /** Runs one execution wave. */
  private def runOneExecution() : Unit = {
    execution = new Execution()
    var hd = activatorQueue
    activatorQueue = lastActivator

    while (hd `ne` lastActivator) {
      execution += hd.procedure
      val nxt = hd.next
      hd.next = null
      hd = nxt
    }

    execution.run()
  }



  /** Automatically initiates a procedure. */
  private[proc] def autoactivate(proc : Procedure) : Unit =
    lock synchronized {
      if (execution == null)
        throw new IllegalStateException("Node remains active outside of execution")
      execution.addImmediate(proc)
    }
}

