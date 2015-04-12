package ru.maxkar.reactive.proc


/**
 * Procedure introspector.
 * Test-level access only.
 */
final object Introspector {
  /** Returns number of registered activation on the procedure.
   * In other words, it is number of "outgoing" links.
   */
  def activationDepCount(proc : Procedure) : Int =
    proc.activations.iterator.size



  /** Returns iterator for procedure dependencies. */
  def iterateActivationDeps(proc : Procedure) : Iterator[Procedure] =
    proc.activations.iterator
}
