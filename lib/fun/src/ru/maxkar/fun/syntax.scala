package ru.maxkar.fun

import scala.language.higherKinds
import scala.language.implicitConversions

/**
 * Functions/monads/applicatives syntax extensions.
 */
package object syntax {

  /** Set of operations over the functor. */
  implicit class FunFunctorExt[S, R](val fn : S ⇒ R) extends AnyVal {
    @inline
    def fmap[T[_]](v : T[S])(implicit fctor : Functor[T]) : T[R] =
      fctor.fmap(fn, v)

    @inline
    def ≻[T[_]](v : T[S])(implicit fctor : Functor[T]) : T[R] = fmap(v)
  }



  /** T-class function application (reverse application). */
  implicit class RevFunFunctorApp[T[_], S, R](val fn : T[S ⇒ R]) extends AnyVal {
    @inline
    def fmap(v : S)(implicit fctor : Functor[T]) : T[R] =
      fn ≺ (ff ⇒ ff(v))

    @inline
    def ≻(v : S)(implicit fctor : Functor[T]) : T[R] = fmap(v)

    @inline
    def fmap(v : T[S])(implicit ap : Applicative[T]) : T[R] =
      ap.aapply(fn, v)

    @inline
    def ≻(v : T[S])(implicit ap : Applicative[T]) : T[R] = fmap(v)
  }



  /** Function to monad application. */
  implicit class FunAsMonadApp[T[_], S, R](val fn : S ⇒ T[R]) extends AnyVal {
    @inline
    def mmap(v : T[S])(implicit md : Monad[T]) : T[R] =
      md.bind(fn, v)

    @inline
    def ≽(v : T[S])(implicit md : Monad[T]) : T[R] = mmap(v)
  }



  /** Boxed function applicable to values. */
  implicit class BoxedMonadicFunctionApp[T[_], S, R](val fn : T[S ⇒ T[R]]) extends AnyVal {
    def mmap(v : T[S])(implicit md : Monad[T]) : T[R] =
     md.flatten(md.aapply(fn, v))

    @inline
    def ≽(v : T[S])(implicit md : Monad[T]) : T[R] = mmap(v)
  }


  /** Magic function application. */
  implicit class BoxedMonadicFunctioAppToV[T[_], S, R](val fn : T[S ⇒ T[R]]) extends AnyVal {
    @inline
    def mmap(v : S)(implicit md : Monad[T]) : T[R] =
      fn ≽ md.pure(v)


    @inline
    def ≽(v : S)(implicit md : Monad[T]) : T[R] = mmap(v)
  }




  /** Converts any object to implicitly fmappable entity. */
  implicit class ObjFuncableExt[T[_], V](val v : T[V]) extends AnyVal {
    @inline
    def rfmap[R](fn : V ⇒ R)(implicit fctor : Functor[T]) : T[R] =
      fctor.fmap(fn, v)

    @inline
    def ≺[R](fn : V ⇒ R)(implicit fctor : Functor[T]) : T[R] = rfmap(fn)

    @inline
    def rmmap[R](fn : V ⇒ T[R])(implicit md : Monad[T]) : T[R] =
      md.bind(fn, v)

    @inline
    def ≼[R](fn : V ⇒ T[R])(implicit md : Monad[T]) : T[R] = rmmap(fn)
  }



  implicit class FlattenableObject[T[_], V](val v : T[T[V]]) extends AnyVal {
    @inline
    def flatten()(implicit md : Monad[T]) : T[V] = md.flatten(v)
  }



  /** Alternative flatten syntax. */
  @inline
  def flatten[T[_], A](x : T[T[A]])(implicit md : Monad[T]) : T[A] =
    md.flatten(x)


  /** Pure constructor. */
  @inline
  def pure[T[_], A](x : A)(implicit app : Applicative[T]) : T[A] =
    app.pure(x)
}
