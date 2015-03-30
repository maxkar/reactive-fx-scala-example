package ru.maxkar.reactive.proc

import ru.maxkar.reactive.deps.Disposable

/**
 * Activation dependency binder for both static and
 * dynamic dependencies. Activation of dependencies causes
 * activation of the procedure related to the binder during the
 * same update turn.
 */
abstract class DependencyBinder {
  /**
   * Activates binder's procedure when <code>proc</code> is
   * activated.
   * @param proc procedure used as an activation trigger.
   * @return binder destructor used to disable created relationship.
   */
  def activateAfter(proc : Procedure) : Disposable
}
