package ru.maxkar.async


import ru.maxkar.fun._
import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._



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
 * @param R result type.
 */
trait Promise[+R] {
  private implicit val ctx = permanentBind

  /** Current promise state. */
  val state : Behaviour[PromiseState[R]]


  /** Infokes a function when promise is complete. */
  final def onComplete(fn : PromiseResult[R] ⇒ Unit) : Unit =
    state ≺ (_ match {
        case a@Success(x) ⇒ fn(a)
        case a@Failure(x) ⇒ fn(a)
        case InProgress ⇒ ()
      })


  /** Invokes a function on promise success. */
  final def onSuccess(cb : R ⇒ Unit) : Unit =
    state ≺ (_ match {
      case a@Success(x) ⇒ cb(x)
      case _ ⇒ ()
    })


  /** Invokes a function on promise failure. */
  final def onFailure(cb : Throwable ⇒ Unit) : Unit =
    state ≺ (_ match {
      case Failure(x) ⇒ cb(x)
      case _ ⇒ ()
    })
}



/**
 * Promise companion object
 */
object Promise {
  import scala.language.implicitConversions
  private implicit val ctx = permanentBind

  /** Creates a new promise from the behaviour. */
  def fromBehaviour[R](b : Behaviour[PromiseState[R]]) : Promise[R] =
    new Promise[R] {
      override val state = b
    }



  /** Creates an async for the non-async result. */
  def immediate[T](v : T) : Promise[T] =
    new Promise[T] {
      override val state = const(Success(v))
    }
}
