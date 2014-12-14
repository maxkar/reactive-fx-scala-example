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
  import scala.language.implicitConversions
  implicit val lifespan = Behaviour.defaultBindContext

  /** Creates a new promise from the behaviour. */
  def fromBehaviour[E, R](b : Behaviour[PromiseState[E, R]]) : Promise[E, R] =
    new Promise[E, R] {
      override val state = b
    }



  /** Creates an async for the non-async result. */
  def immediate[E, T](v : T) : Promise[E, T] =
    new Promise[E, T] {
      override val state = const(Success(v))
    }




  /** Monadic flatten. */
  def flatten[E, R](v : Promise[E, Promise[E, R]]) : Promise[E, R] =
    fromBehaviour(Behaviour.join(v.state :< (vv ⇒ vv match {
      case Failure(x) ⇒ Behaviour.const(Failure(x))
      case InProgress ⇒ Behaviour.const(InProgress)
      case Success(x) ⇒ x.state
    })))



  implicit class MapFnOverPromise[T, R](val fn : T ⇒ R) extends AnyVal {
    def :>[E](promise : Promise[E, T]) : Promise[E, R] =
      fromBehaviour(promise.state :< (r ⇒ r.map(fn)))
  }



  implicit class ApplicativeFnOverPromise[E, T, R](val fn : Promise[E, T ⇒ R]) extends AnyVal {
    def :>(promise : Promise[E, T]) : Promise[E, R] = {
      var fail : Failure[E] = null
      def combo(r1 : PromiseState[E, T ⇒ R])(r2 : PromiseState[E, T]) : PromiseState[E, R] = {
        if (fail != null)
          return fail
        (r1,r2) match {
          case (Failure(x), _) ⇒
            fail = Failure(x)
            return fail
          case (_, Failure(x)) ⇒
            fail = Failure(x)
            return fail
          case (InProgress, _) | (_, InProgress) ⇒ InProgress
          case (Success(x), Success(y)) ⇒ Success(x(y))
        }
      }
      fromBehaviour(combo _ :> fn.state :> promise.state)
    }


    def :>(v : T) : Promise[E, R] =
      fromBehaviour(fn.state :< (x ⇒ x.map(z ⇒ z(v))))
  }



  implicit class MonadicFnApply[E, T, R](val fn : Promise[E, T ⇒ Promise[E, R]]) extends AnyVal {
    def :>>(promise : Promise[E, T]) : Promise[E, R] =
      flatten(fn :> promise)
  }


  implicit class MonadicPureFnApply[E, T, R](val fn : T ⇒ Promise[E, R]) extends AnyVal {
    def :>>(promise : Promise[E, T]) : Promise[E, R] =
      flatten(fn :> promise)
  }
}
