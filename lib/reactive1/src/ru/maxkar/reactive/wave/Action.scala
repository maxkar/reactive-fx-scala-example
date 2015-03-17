package ru.maxkar.reactive.wave

import ru.maxkar.reactive.iter._

/**
 * Action processor implementation.
 * @param S action aggregation state.
 * @param I action input (request) state.
 * @param O action output (event) state.
 * @param default default value for the node.
 * @param factoryf function used to create new internal state from input.
 * @param foldf function used to update internal state when many actions
 *   are performed in same "transaction".
 * @param finishf function used to finish processing and generate action
 *   result.
 */
private[wave] final class Action[S, I, O](
      default : O,
      factoryf : I ⇒ S,
      foldf : (S, I) ⇒ S,
      finishf : S ⇒ O) {

  /** Internal action state. */
  private var state : Option[S] = None



  /** State of the action inside a wave. */
  private var wavedState : Option[S] = None



  /** Action node. */
  private val pin = new Pin[O](default, emptyIter, finish)



  /**
   * Initiates/updates an action using given input.
   * Initiates new action (and wave) if this action is not
   * active. Otherwise just adds an action to be executed.
   */
  def initiate(input : I) : Unit =
    Wave.batch {
      state match {
        case Some(x) ⇒
          state = Some(foldf(x, input))
        case None ⇒
          state = Some(factoryf(input))
          Wave.addAction(this)
      }
    }



  /**
   * Prefetches action and sets it as "ready-for-wave".
   * @return pin used for dependency tracking.
   */
  private[wave] def prepare() : Pin[O] = {
    wavedState = state
    state = None
    pin
  }



  /**
   * Finishes a wave.
   */
  private def finish() : O =
    wavedState match {
      case Some(x) ⇒
        wavedState = None
        finishf(x)
      case None ⇒
        throw new IllegalStateException("Attemt to finish inactive action")
    }
}
