package ru.maxkar.reactive.proc

/**
 * Procedure evaluation activator.
 * @param procedure procedure activated by this activator.
 */
final class Activator private[proc](
      private[proc] val procedure : Procedure) {

  /** Activator for next procedure. */
  private[proc] var next : Activator = null



  /** Activates related procedure for the execution. */
  def activate() : Unit =
    Execution.activate(this)
}



/**
 * Activator companion.
 */
object Activator {
  /** Executes batch operation. Each procedure will be activated
   * at most once as a result of the batch.
   */
  def batch(cb : ⇒ Unit) : Unit =
    Execution.batch(cb)
}
