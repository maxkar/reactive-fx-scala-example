package ru.maxkar.async

import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.reactive.value._

/**
 * Controller/processor of the simple attractor.
 * @param V type of "goal" value.
 * @param R type of function result.
 * @param fn function to be applied to current goal.
 * @param op asyng operation provider.
 * @param bindCtx binding and creation context.
 */
private[async] final class SimpleAttractor[V, R](fn : V ⇒ R, op : Promising) {

  /** Current state of the attractor. */
  private val state : Variable[AttractorState[R]] = variable(null)



  /** Next attractor goal. */
  private var nextGoal : Option[V] = None



  /** Attractor associated with this implementation. */
  private[async] def attractor : Attractor[R] = state.behaviour



  /** Sets a new attractor goal. */
  private[async] def setGoal(goal : V) : Unit =
    nextGoal match {
      case Some(x) ⇒ nextGoal = Some(goal)
      case None ⇒
        attractor.value match {
          case Updating ⇒ nextGoal = Some(goal)
          case _ ⇒ approach(goal)
        }
    }



  /** Approaches the goal by applying function to it. */
  private def approach(goal : V) : Unit = {
    state.set(Updating)
    op(fn(goal)).onComplete(updateAfterFunction)
  }



  /** Updates attractor state after one goal is reached. */
  private def updateAfterFunction(res : PromiseResult[R]) : Unit =
    nextGoal match {
      case Some(x) ⇒
        nextGoal = null
        approach(x)
      case None ⇒
        res match {
          case Failure(f) ⇒ state.set(Failure(f))
          case Success(r) ⇒ state.set(Success(r))
        }
    }
}
