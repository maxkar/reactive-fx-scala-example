package ru.maxkar.async

/**
 * Base type for a promise result.
 * @param E error type.
 * @param R result type.
 */
abstract sealed class PromiseState[+E, +R]


/**
 * Promise result state. Promise is completed in this state.
 */
abstract sealed class PromiseResult[+E, +R] extends PromiseState[E, R]


/** State where operation is not complete yet. */
final case object InProgress extends PromiseState[Nothing, Nothing]


/** Operation failure state. */
final case class Failure[E](error : E) extends PromiseResult[E, Nothing]

/** Operation success state. */
final case class Success[R](result : R) extends PromiseResult[Nothing, R]
