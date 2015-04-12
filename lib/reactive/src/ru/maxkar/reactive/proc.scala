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



  /**
   * Abstract definition of an actions and dependencies.
   * Installs activation dependencies and creates a new action after the call.
   * <p>Users are encouraged to use standard specification constructors
   *   provided by the library (basic, collection, etc...) because
   *   specification API could change in future versions.
   * <p>Binder should not be called outside of compilation (i.e.
   *  during current call to <code>compile</code>) and action (i.e. call
   *  to Action.start() or Process.processTillNextProcedure).
   */
  type Specification = DepBinder ⇒ Action



  /**
   * Composable part of the procedure. Each action could
   * be used as a part of one procedure and each procedure could
   * internally consists of many actions.
   * <p>Each action creates a new calculation process upon a call.
   */
  type Action = () ⇒ Process



  /**
   * Procedure execution process. Something very similar to coroutine
   * or coprocedure. Proceeds sequentially until it have to wait until
   * next procedure is complete.
   * <p>On each call this function proceedes until this process have to wait
   * until active process for some other procedure completes.
   * Returns that procedure to await. Next call
   * to this method will be made only after given procedure is complete
   * for this tick (i.e. it could not be run or completed).
   * Return <code>null</code> after process completion.
   */
  type Process = () ⇒ Procedure
}
