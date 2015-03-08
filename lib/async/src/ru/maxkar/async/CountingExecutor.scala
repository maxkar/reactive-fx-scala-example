package ru.maxkar.async

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import java.util.concurrent._
import Runnables._

/**
 * Executor which counts number of active operations.
 * @param runLater platform-level exit used to unlink behaviour-based updates on counter.
 */
final class CountingExecutor(runLater : Runnable ⇒ Unit, peer : Promising)
    extends Promising {


  /** Number of operations in this executor. */
  private val opCountV = variable(0)

  /** Number of active operations in this executor. */
  val operationCount : Behaviour[Int] = opCountV


  override def apply[T](op : ⇒ T) : Promise[T] = {
    val base = peer(op)
    base.value match {
      case Failure(_) ⇒ ()
      case _ ⇒
        runLater(run { opCountV.set(opCountV.value + 1) })
        base.onComplete(res ⇒
          runLater(run { opCountV.set(opCountV.value - 1) }))
    }
    base
  }
}
