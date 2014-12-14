package ru.maxkar.fun

import scala.language.higherKinds

/** Applicative type class. */
trait Applicative[T[_]] extends Functor[T] {
  def pure[V](v : V) : T[V]
  def aapply[S, R](fn : T[S ⇒ R], v : T[S]) : T[R]


  /** Default fmap operation. */
  override def fmap[S, R](fn : S ⇒ R, item : T[S]) : T[R] =
    aapply(pure(fn), item)
}
