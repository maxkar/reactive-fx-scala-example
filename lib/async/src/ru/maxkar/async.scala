package ru.maxkar


import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.reactive.value._


/**
 * Advanced options for async operations.
 */
package object async {
  private implicit val ctx = permanentBind

  implicit object PromiseAsMonad extends Monad[Promise] {
    override def pure[V](v : V) : Promise[V] =
      Promise.immediate(v)


    override def bind[S, R](fn : S ⇒ Promise[R], v : Promise[S]) : Promise[R] =
      Promise.fromBehaviour(ctx.flatten(v.state ≺ (vv ⇒ vv match {
        case Failure(x) ⇒ const(Failure(x))
        case InProgress ⇒ const(InProgress)
        case Success(x) ⇒ fn(x).state
      })))
  }
}
