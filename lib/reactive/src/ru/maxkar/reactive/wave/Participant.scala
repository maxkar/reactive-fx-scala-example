package ru.maxkar.reactive.wave

import scala.collection.mutable.Queue

/**
 * Node for participating inside the flow. This node
 * is used to track resolution order between different
 * components in each flow.
 */
final class Participant private[wave](
      onBoot : Wave ⇒ Unit,
      onResolved : Wave ⇒ Unit,
      onCleanup : () ⇒ Unit) {

  /**
   * Nodes correlated to this participant. That correlated nodes should
   * be engaged if this node is engaged.
   */
  private val correlatedNodes = new java.util.ArrayList[Participant]



  /**
   * Functions to ivoke before this wave node is finally resolved.
   * These function may install additional deferrances and dependencies.
   */
  private val preResolutionListeners = new Queue[Wave ⇒ Unit]



  /**
   * Queue of functions to invoke when this node is resolved.
   */
  private val resolutionListeners = new Queue[Participant]



  /**
   * Number of pending dependencies.
   */
  private var pendingDeps = 0



  /**
   * Code for the current state.
   */
  private var state = Participant.STATE_READY



  /* WAVE CLIENT SECTION. */

  /**
   * Registers a node as correlated to this node.  This method creates
   * uni-directional relationship: if <code>this</code> node is added to
   * a wave, then <code>corr</code> node is added to the same wave.
   * <p>Correlation does not establish any sort of "order" between nodes.
   * So <code>corr</code> node may be resolved before <code>this</node>.
   * @param node node correlated to this.
   */
  def addCorrelatedNode(corr : Participant) : Unit =
    correlatedNodes.add(corr)



  /**
   * Removes correlation between <code>this</code> node and <code>corr</code>
   * node. Does nothing if there is no correlation. If correlation was
   * established several times, removes one correlation only.
   */
  def removeCorrelatedNode(corr : Participant) : Unit =
    correlatedNodes.remove(corr)



  /**
   * Attempts to engage this participant into the target wave. Each node
   * must participate in one wave only.
   * @param wave wave to join to.
   * @return <code>true</code> iff this node was added to the wave.
   * @throws IllegalStateException if wave is advanced after the engagement
   * phase.
   */
  def engage(implicit wave : Wave) : Unit = {
    if (state == Participant.STATE_READY) {
      state = Participant.STATE_ENGAGED
      wave.enqueueParticipant(this)
    }
    else if (state != Participant.STATE_ENGAGED)
      throw new IllegalStateException(
        "This node is in incorrect engagement position")
  }


  /**
   * Immediately marks this node as engaged. Used by wave to
   * mark a participant as active and under the resolution.
   */
  private[wave] def innerEngage() : Unit =
    state = Participant.STATE_ENGAGED



  /**
   * Adds a dependency between nodes in a way that this node will
   * be resolved only after the dependent node is resolved.
   * @param target "previous" node before this node.
   * @return this node.
   * @throws IllegalStateException if this node is not engaged
   * in any wave.
   * @throws IllegalStateException if target node is engaged in different * wave.
   */
  def defer(target : Participant) : this.type = {
    if (state != Participant.STATE_ENGAGED)
      throw new IllegalStateException(
        "Cannot defer non-engaged node, state is " + state)

    /* Either already resolved or not in this wave. */
    if (target.state != Participant.STATE_ENGAGED)
      return this

    pendingDeps += 1
    target.resolutionListeners += this
    this
  }



  /**
   * Adds a dependency between nodes along with the callback
   * to invoke when all the nodes are resolved.
   * @param callback callback to invoke when previous node is resolved.
   * This callback can create additional dependencies (further deferrances)
   * for this node.
   * @param target "previous" node before this node.
   * @throws IllegalStateException if this node is not engaged
   * in any wave.
   * @throws IllegalStateException if target node is not engaged in the wave.
   */
  def deferCb(callback : (Wave) ⇒ Unit, target : Participant) : this.type = {
    invokeBeforeResolve(callback)
    defer(target)
  }



  /**
   * Schedules a function to be invoked before the resolution. This function
   * will be invoked only all existing defferences are resolved. So it is
   * guaranteed that in the following code <code>fn</code> will be called only
   * when <code>nd</code> is resovled.
   * <code>
   *   node.defer(nd)
   *   invokeBeforeResolve(fn)
   * </code>
   * That code is save even that sequence happens inside any participant callback
   * (boot or previously registered function).
   * @param callback callback to invoke.
   * @throws IllegalStateException if target node is not engaged in the wave.
   */
  def invokeBeforeResolve(callback : (Wave) ⇒ Unit) : this.type = {
    if (state != Participant.STATE_ENGAGED)
      throw new IllegalStateException(
        "Cannot schedule invokation for non-engaged node, state is " + state)
    preResolutionListeners += callback
    this
  }



  /* QUEUE INTEGRATION API. */

  /**
   * Handles an engagement event. Adds all correlated nodes into the flow
   * but does not attempts to resolve anything because some nodes
   * may be out of the flow. Gives a chance to participate in the wave to
   * all the dependencies.
   * @param wave current wave.
   */
  private[wave] def engageComplete(wave : Wave) : Unit = {
    val itr = correlatedNodes.iterator()
    while (itr.hasNext())
      itr.next().engage(wave)
  }



  /**
   * Bootstraps this wave. This is the first attempt to resolve the
   * node after wave completed initial engagement (so no other nodes
   * will be added into the propagation).
   */
  private[wave] def boot(wave : Wave) : Unit = {
    onBoot(wave)

    /* Try to resolve immediately if possible. */
    tryResolve(wave)
  }



  /**
   * Attempts to resolve this node. This method will run all listeners
   * which was registered during calls do defer* methods. That method
   * may install additional dependencies. If there are no unsatisfied
   * dependencies at the end of this process, this node is considered
   * "resolved" and added into the cleanup queue. Otherwise this node
   * will wait until all dependencies are resolved.
   * @param wave current wave.
   */
  private[wave] def tryResolve(wave : Wave) : Unit = {
    /* TODO: Copy into another batch? This may stop a little bit
     * earlier than allowed.
     */
    while (pendingDeps == 0 && !preResolutionListeners.isEmpty)
      preResolutionListeners.dequeue()(wave)

    /* New deps were discovered, return. */
    if (pendingDeps > 0)
      return

    state = Participant.STATE_RESOLVED
    onResolved(wave)
    wave.enqueueResolved(this)
  }



  /**
   * Handles an event where one of dependencies was resolved.
   * If all the dependencies are resolved, attempts to resolve this
   * node. If node is resolved, it will be enqueued to notify other
   * nodes about it's completion.
   * @param wave current wave.
   */
  private def depResolved(wave : Wave) : Unit = {
    pendingDeps -= 1

    if (pendingDeps == 0)
      tryResolve(wave)
  }



  /**
   * Resets dependencies for all listeners.
   * @param wave current wave.
   */
  private[wave] def notifyDeps(wave : Wave) : Unit =
    while (!resolutionListeners.isEmpty)
      resolutionListeners.dequeue.depResolved(wave)



  /**
   * Cleans this node after the wave propagation.
   */
  private[wave] def cleanup() : Unit = {
    state = Participant.STATE_READY
    onCleanup()
  }
}




/** Wave participant companion. */
final object Participant {
  /** Participant is ready to be engaged. */
  private val STATE_READY = 0

  /** Participant is engaged in the wave. */
  private val STATE_ENGAGED = 1


  /** Node is resolved during the propagation. */
  private val STATE_RESOLVED = 2
}

