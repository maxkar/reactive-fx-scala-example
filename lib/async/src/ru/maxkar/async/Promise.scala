package ru.maxkar.async

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/**
 * Simple asynchronous operation for single-threaded
 * execution flow (like EDT in UI). Promise do not track
 * operation progress, it just indicates if operation is
 * finished or not.
 * <p>Only one transition is allowed from InProgress to
 * either success or failure.
 * <p>All access to promise should be performed in the allowed
 * thread only (executor thread, EDT thread or some other
 * thread indicated by the promise factory.
 * @param E error type.
 * @param R result type.
 */
trait Promise[+E, +R] {
  private implicit val ctx = defaultBindContext

  /** Current promise state. */
  val state : Behaviour[PromiseState[E, R]]


  /** Infokes a function when promise is complete. */
  final def onComplete(fn : PromiseResult[E, R] ⇒ Unit) : Unit =
    state :< (_ match {
        case a@Success(x) ⇒ fn(a)
        case a@Failure(x) ⇒ fn(a)
        case InProgress ⇒ ()
      })


  /** Invokes a function on promise success. */
  final def onSuccess(cb : R ⇒ Unit) : Unit =
    state :< (_ match {
      case a@Success(x) ⇒ cb(x)
      case _ ⇒ ()
    })


  /** Invokes a function on promise failure. */
  final def onFailure(cb : E ⇒ Unit) : Unit =
    state :< (_ match {
      case Failure(x) ⇒ cb(x)
      case _ ⇒ ()
    })
}



/**
 * Promise companion object
 */
object Promise {
  implicit val lifespan = Lifespan.forever

  /** Creates a new promise from the behaviour. */
  def fromBehaviour[E, R](b : Behaviour[PromiseState[E, R]]) : Promise[E, R] =
    new Promise[E, R] {
      override val state = b
    }



  def immediate[E, T](v : T) : Promise[E, T] =
    new Promise[E, T] {
      override val state = const(Success(v))
    }
}
