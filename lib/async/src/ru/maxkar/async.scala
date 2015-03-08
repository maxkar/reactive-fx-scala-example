package ru.maxkar


import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.reactive.value._


/**
 * Advanced options for async operations.
 */
package object async {
  import scala.language.implicitConversions



  private implicit val ctx = permanentBind



  /** Promise type. */
  type Promise[T] = Behaviour[PromiseState[T]]



  /** Attractor state. This is asynchronous behaviour which could recalcutale
   * at some times.
   */
  type Attractor[T] = Behaviour[AttractorState[T]]



  /** Monad for the promise. */
  implicit object PromiseAsMonad extends Monad[Promise] {
    override def pure[V](v : V) : Promise[V] = immediate(v)


    override def bind[S, R](fn : S ⇒ Promise[R], v : Promise[S]) : Promise[R] =
      ctx.bind((vv : PromiseState[S]) ⇒ vv match {
        case Failure(x) ⇒ const[PromiseState[R]](Failure(x))
        case InProgress ⇒ const[PromiseState[R]](InProgress)
        case Success(x) ⇒ fn(x)
      }, v)
  }


  implicit class PromiseOps[R](val v : Promise[R]) extends AnyVal {
    def onComplete(cb : PromiseResult[R] ⇒ Unit) : Unit =
      ctx.fmap((st : PromiseState[R]) ⇒ st match {
        case a@Failure(x) ⇒ cb(a)
        case a@Success(x) ⇒ cb(a)
        case _ ⇒ ()
      }, v)


     @inline
     def onSuccess(cb : R ⇒ Unit) : Unit = cb ≻ v


     def onFailure(cb : Throwable ⇒ Unit) : Unit =
       ctx.fmap((st : PromiseState[R]) ⇒ st match {
         case Failure(x) ⇒ cb(x)
         case _ ⇒ ()
       }, v)
  }


  /** Creates an "immediate" (resolved) promise. */
  def immediate[V](v : V) : Promise[V] = const(Success(v))



  /**
   * Creates a simple "attractor". Attractor is a process which tries to
   * asynchronously apply function to a given value and present result of
   * that function as a result. Due to nature of the operation, input value
   * could change while calculation is still in progress. In this case
   * attractor will reapply function to a new value when previous operation
   * is complete. Value can change several times during one async operation.
   * Attractor will try to reapply function only to a latest goal, all
   * "intermediate" goals will be lost.
   * @param V input value type
   * @param R output value type
   * @param v input value state. This is always as "target" and attractor will
   *   try to apply function to a latest value of <code>v</code>
   * @param fn function to apply to the value.
   * @param ctx async operation provide. All async operations will be
   *   performed using that provider.
   * @param bindCtx attractor binding context.
   * @return "attractor" representing state of asynchronously applying
   *  <code>fn</code> to value of <code>v</code>.
   */
  def simpleAttractor[V, R](
        v : Behaviour[V], fn : V ⇒ R,
        ctx : Promising)(
        implicit bindCtx : BindContext)
      : Attractor[R] = {
    val res = new SimpleAttractor(fn, ctx)
    v ≺ res.setGoal
    res.attractor
  }
}
