package ru.maxkar.async

/**
 * Base type for a promise result.
 * @param R result type.
 */
abstract sealed class PromiseState[+R] {
  def map[T](fn : R ⇒ T) : PromiseState[T]
}


/**
 * Type of attractor state.
 */
abstract sealed trait AttractorState[+R]


/**
 * Promise result state. Promise is completed in this state.
 */
abstract sealed class PromiseResult[+R] extends PromiseState[R]


/** State where operation is not complete yet. */
final case object InProgress
    extends PromiseState[Nothing]{
  def map[T](fn : Nothing ⇒ T) = InProgress
}


/** Attractor is updating right now. */
final case object Updating extends AttractorState[Nothing]


/** Operation failure state. */
final case class Failure(error : Throwable)
    extends PromiseResult[Nothing]
    with AttractorState[Nothing] {
  def map[T](fn : Nothing ⇒ T) = this
}

/** Operation success state. */
final case class Success[R](result : R)
    extends PromiseResult[R]
    with AttractorState[R] {
  def map[T](fn : R ⇒ T) = Success(fn(result))
}
