package ru.maxkar.reactive.proc.spec

import ru.maxkar.reactive.proc._


/** Dynamic binding action.
 * @param binder downstream bind manipulator.
 * @param provider procedure provider.
 */
private[spec] final class DynBindAction(
      binder : DownstreamDeps,
      provider : () ⇒ Procedure)
    extends Action {

  /** Last used dependency. */
  private var lastDep = provider()

  /** Last (active) binding. */
  private var lastBinding = binder += lastDep



  override def start() : Process =
    new Process {
      private var finished = false

      override def proceedTillNextProcedure() : Procedure = {
        if (finished)
          return null

        finished = true

        val newDep = provider()
        if (newDep `ne` lastDep) {
          lastDep = newDep
          lastBinding.dispose()
          lastBinding = binder += newDep
        }
        lastDep
      }
    }
}
