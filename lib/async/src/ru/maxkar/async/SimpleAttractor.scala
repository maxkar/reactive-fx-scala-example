package ru.maxkar.async

import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.reactive.value._
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.spec.Specs

/**
 * Controller/processor of the simple attractor.
 * @param V type of "goal" value.
 * @param R type of function result.
 * @param base attractor goal.
 * @param fn function to be applied to current goal.
 * @param op asyng operation provider.
 * @param ctx binding and creation context.
 */
private[async] final class SimpleAttractor[V, R](
      private[async] val base : Behaviour[V], fn : V ⇒ R,
      op : Promising, ctx : BindContext) {

  /** Current goal for the attractor. */
  private var currentGoal = base.value

  /** Current process value. */
  private[async] var currentProcess = op(fn(currentGoal))

  /** Curernt result. */
  private var currentRes = toAttractorState(currentProcess.value)

  /** Change flag. */
  private var changed = false


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
        nextGoal = None
        approach(x)
      case None ⇒
        res match {
          case Failure(f) ⇒ state.set(Failure(f))
          case Success(r) ⇒ state.set(Success(r))
        }
    }



  /** Converts promise into attractor state. */
  private def toAttractorState[R](st : PromiseState[R]) : AttractorState[R] =
    st match {
      case InProgress ⇒ Updating
      case Success(x) ⇒ Success(x)
      case Failure(x) ⇒ Failure(x)
    }
}
