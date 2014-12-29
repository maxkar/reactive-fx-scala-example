package ru.maxkar.reactive.value

import ru.maxkar.fun._

import ru.maxkar.reactive.wave.Participable

/**
 * Context used for value binding. Holds both
 * lifecycle context for the binding and current
 * update wave (if any).
 * @param lifespan bind lifetime context.
 * @param update update participation context.
 */
final class BindContext(
      val lifespan : Lifespan,
      val update : Participable)
    extends Monad[Behaviour] {

  /* Functor implementation (more effective than default). */

  override def fmap[S, R](fn : S ⇒ R, item : Behaviour[S]) : Behaviour[R] = {
    val res = new MapBehaviour(fn, item, this)
    lifespan.onDispose(res.dispose)
    res
  }



  /* Applicative implementation. */

  override def pure[T](v : T) : Behaviour[T] = const(v)

  override def aapply[S, R](fn : Behaviour[S ⇒ R], v : Behaviour[S]) : Behaviour[R] = {
    val res = new ApplicativeBehaviour(fn, v, this)
    lifespan.onDispose(res.dispose)
    res
  }


  /* Monad implementation. */

  override def flatten[V](x : Behaviour[Behaviour[V]]) : Behaviour[V] = {
    val res = new Flatten(x, this)
    lifespan.onDispose(res.dispose)
    res
  }


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
      : Behaviour[R] = {
    var res = new SubmodelDispatch(fn, v, this)
    lifespan.onDispose(res.dispose)
    res
  }


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
    flatten(subfmap(fn, v))
}
