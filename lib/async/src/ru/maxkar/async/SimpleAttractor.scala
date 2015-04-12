package ru.maxkar.async

import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.reactive.value._
import ru.maxkar.reactive.proc._

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
      op : Promising, ctx : BindContext)
    extends Behaviour[AttractorState[R]] {

  /** Current goal for the attractor. */
  private var currentGoal = base.value

  /** Current process value. */
  private[async] var currentProcess = op(fn(currentGoal))

  /** Curernt result. */
  private var currentRes = toAttractorState(currentProcess.value)

  /** Change flag. */
  private var changed = false


  private val proc =
    Procedure.compile(
      new Specification {
        override def compile(dep : DepBinder) : Action = {
          dep += base.change.procedure

          Action.seq(
            Action.await(base.change.procedure),
            Action.forSingle(currentProcess.change.procedure),
            Action.forUnit { updateLoading() },
            Action.dynamicBindTo(dep, currentProcess.change.procedure),
            Action.forUnit { updateValue() } )
        }
      },
      ctx.binder,
      () ⇒ changed = false)


  /** Updates loading state and returns its procedure. */
  private def updateLoading() : Unit = {
    if (currentProcess.value == InProgress)
      return

    if (currentGoal != base.value) {
      currentGoal = base.value
      currentProcess = op(fn(currentGoal))
    }
  }



  /** Updates value of this attractor. */
  private def updateValue() : Unit = {
    val ns = toAttractorState(currentProcess.value)

    if (currentRes != ns) {
      changed = true
      currentRes = ns
    }
  }



  /** Converts promise into attractor state. */
  private def toAttractorState[R](st : PromiseState[R]) : AttractorState[R] =
    st match {
      case InProgress ⇒ Updating
      case Success(x) ⇒ Success(x)
      case Failure(x) ⇒ Failure(x)
    }


  /* BEHAVIOUR IMPLEMENTATION. */
  override def value() : AttractorState[R] = currentRes
  override val change = Signal[Boolean](proc, changed)
}
