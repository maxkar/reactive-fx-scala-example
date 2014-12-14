package ru.maxkar


import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/**
 * Advanced options for async operations.
 */
package object async {
  private implicit val lifespan = Behaviour.defaultBindContext

  implicit object PromiseAsMonad extends Monad[Promise] {
    override def pure[V](v : V) : Promise[V] =
      Promise.immediate(v)


    override def bind[S, R](fn : S ⇒ Promise[R], v : Promise[S]) : Promise[R] =
      Promise.fromBehaviour(Behaviour.join(v.state :< (vv ⇒ vv match {
        case Failure(x) ⇒ Behaviour.const(Failure(x))
        case InProgress ⇒ Behaviour.const(InProgress)
        case Success(x) ⇒ fn(x).state
      })))
  }
}
