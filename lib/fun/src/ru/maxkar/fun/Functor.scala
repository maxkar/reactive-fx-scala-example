package ru.maxkar.fun

import scala.language.higherKinds

/**
 * High-order functor typeclass.
 * @param T type of the functor container.
 */
trait Functor[T[_]] {
  /** General functor "fmap" contact. */
  def fmap[S, R](fn : S ⇒ R, item : T[S]) : T[R]
}
