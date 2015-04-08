package ru.maxkar.reactive.value

import ru.maxkar.fun._

import ru.maxkar.reactive.Disposable
import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.spec.Specs


/**
 * Reactive binding context. Also implements monadic bindings for
 * reactive programming.
 * @param binder model binder used in this context.
 */
final class BindContext(binder : Binder)
    extends Monad[Behaviour] {


  /** Creates a sub-context. This new context could be disposed manually or
   * by destroying this context manually.
   * @returns pair of new bind context and its destructor.
   */
  def sub() : (BindContext, Disposable) = {
    val (nb, dest) = binder.sub
    (new BindContext(nb), dest)
  }



  /* Functor implementation (more effective than default). */
  override def fmap[S, R](fn : S ⇒ R, item : Behaviour[S]) : Behaviour[R] =
    new MapBehaviour(binder, fn, item)



  /* Applicative implementation. */

  override def pure[T](v : T) : Behaviour[T] = const(v)

  override def aapply[S, R](fn : Behaviour[S ⇒ R], v : Behaviour[S]) : Behaviour[R] =
    new ApplicativeBehaviour(binder, fn, v)

  override def flatten[V](x : Behaviour[Behaviour[V]]) : Behaviour[V] =
    new Flatten(binder, x)

  override def bind[S, R](fn : S ⇒ Behaviour[R], v : Behaviour[S]) : Behaviour[R] =
    flatten(fmap(fn, v))



  /* Lifecycle/subcontext binding. */

  /**
   * Value-based submodel dispatch. Creates a new
   * submodel which lifetime is bound to lifetime
   * of <em>current</em> value of this behaviour.
   * When value changes, subcontext would be disposed and
   * new subcontext would be created allowing for a
   * completely new model.
   */
  def subfmap[S, R](
        fn : (S, BindContext) ⇒ R,
        v : Behaviour[S])
      : Behaviour[R] =
    new ShallowSubmodelDispatch(binder, fn, v)



  /**
   * Value-based submodel dispatch. Creates a new
   * submodel which lifetime is bound to lifetime
   * of <em>current</em> value of this behaviour.
   * When value changes, subcontext would be disposed and
   * new subcontext would be created allowing for a
   * completely new model.
   */
  def submmap[S, R](
        fn : (S, BindContext) ⇒ Behaviour[R],
        v : Behaviour[S])
      : Behaviour[R] =
    new SubmodelDispatch(binder, fn, v)
}
