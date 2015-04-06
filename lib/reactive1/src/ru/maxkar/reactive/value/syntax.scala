package ru.maxkar.reactive.value


/**
 * Syntax extensions for subcontext/submonad bindings.
 */
package object syntax {
  import scala.language.implicitConversions


  implicit class FnToSubfmap[S, R](val fn : (S, BindContext) ⇒ R) extends AnyVal {
    @inline
    def subfmap(v : Behaviour[S])(implicit ctx : BindContext) : Behaviour[R] =
      ctx.subfmap(fn, v)

    @inline
    def ~≻ (v : Behaviour[S])(implicit ctx : BindContext) : Behaviour[R] =
      subfmap(v)(ctx)
  }


  implicit class FnToSubmap[S, R](val fn : (S, BindContext) ⇒ Behaviour[R]) extends AnyVal {
    @inline
    def submmap(v : Behaviour[S])(implicit ctx : BindContext) : Behaviour[R] =
      ctx.submmap(fn, v)


    @inline
    def ~≽ (v : Behaviour[S])(implicit ctx : BindContext) : Behaviour[R] =
      submmap(v)(ctx)
  }


  implicit class RevFnSumbap[V](val v : Behaviour[V]) extends AnyVal {
    @inline
    def rsubmmap[R](
          fn : (V, BindContext) ⇒ Behaviour[R])(
          implicit ctx : BindContext)
        : Behaviour[R] =
      ctx.submmap(fn, v)


    @inline
    def ≼~ [R](
          fn : (V, BindContext) ⇒ Behaviour[R])(
          implicit ctx : BindContext)
        : Behaviour[R] =
      rsubmmap(fn)(ctx)


    @inline
    def rsubfmap[R](
          fn : (V, BindContext) ⇒ R)(
          implicit ctx : BindContext)
        : Behaviour[R] =
      ctx.subfmap(fn, v)



    @inline
    def ≺~ [R](
          fn : (V, BindContext) ⇒ R)(
          implicit ctx : BindContext)
        : Behaviour[R] =
      rsubfmap(fn)(ctx)
  }
}

