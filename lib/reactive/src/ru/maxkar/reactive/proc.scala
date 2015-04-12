package ru.maxkar.reactive

/**
 * Procedure execution package.
 */
package object proc {
  /** Procedure state. */
  private[proc] abstract sealed class State

  /** Procedure is passive (not executing). */
  private[proc] case object Passive extends State

  /** Procedure was activated but is not executing yet. */
  private[proc] case object Active extends State

  /** Procedure is complet during current tick. */
  private[proc] case object Complete extends State
}
