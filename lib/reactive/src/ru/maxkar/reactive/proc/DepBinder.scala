package ru.maxkar.reactive.proc

import ru.maxkar.reactive.Disposable
import ru.maxkar.reactive.deps.Binder

/**
 * Activation dependency binder for both static and
 * dynamic dependencies. Activation of dependencies causes
 * activation of the procedure related to the binder during the
 * same update turn.
 * @param procedure procedure to use during bindings.
 * @param binder dependency binder.
 */
final class DepBinder private[proc](
        procedure : Procedure,
        binder : Binder) {

  /** Flag inditacting a need to automatically activate procedure. */
  private var autoactivate : Boolean = true


  /**
   * Activates binder's procedure when <code>proc</code> is
   * activated.
   * @param proc procedure used as an activation trigger.
   * @return binder destructor used to disable created relationship.
   */
  def += (proc : Procedure) : Disposable = {
    if (autoactivate && proc.isActive) {
      autoactivate = false
    }
    binder.bind(proc.activations, procedure)
  }



  /** Completes initialization. Returns <code>true</code> iff procedure
   * should be activated automatically.
   */
  private[proc] def completeInit() : Boolean = {
    val res = !autoactivate
    autoactivate = false
    res
  }
}
