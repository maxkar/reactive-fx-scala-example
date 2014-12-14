package ru.maxkar.fun

import scala.language.higherKinds

/** General-purpose monad definition. */
trait Monad[T[_]] extends Applicative[T] {
  /** Monadic bind. */
  def bind[S, R](fn : S ⇒ T[R], v : T[S]) : T[R]


  /** Default applicative. */
  override def aapply[S, R](fn : T[S ⇒ R], v : T[S]) : T[R] =
    bind(
      (x : S ⇒ R) ⇒
        bind(
          (a : S) ⇒ pure(x(a)),
          v),
      fn)


  /** Monad flattening, default implementation. */
  def flatten[V](x : T[T[V]]) : T[V] =
    bind((a : T[V]) ⇒ a, x)
}
