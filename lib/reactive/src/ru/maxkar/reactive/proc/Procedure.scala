package ru.maxkar.reactive.proc

import ru.maxkar.reactive.deps.Binder
import ru.maxkar.reactive.deps.DependencyList


/**
 * Series of actions and singe "manageable" unit of work.
 * Could execute multiple actions and serves as a dependency unit.
 * Actions could depend on procedure only but not on individual
 * actions.
 * @param spec action specification.
 * @param binder action dependency binder.
 * @param onCleanup action cleanup listener.
 */
final class Procedure private(
      spec : Specification,
      binder : Binder,
      onCleanup : () ⇒ Unit = null) {

  /** Next field in the procedure queue. Managed by procList. */
  private[proc] var next : Procedure = null



  /** Active process in this procedure. */
  private var process : Process = null



  /** Queue of procedures awaiting resolution of this procedure. */
  private[proc] val deps = new ProcList()



  /** List of procedures which should be activated after this procedure
   * is activated. */
  private[proc] val activations = new DependencyList[Procedure]()



  /** Action used to create processes. */
  private val action = {
    val db = new DownstreamDeps(this, binder)
    val act = spec.compile(db)
    if (db.completeInit())
      Execution.autoactivate(this)
    act
  }



  /** Checks if this procedure is active. */
  def isActive() : Boolean = process != null



  /** Activates a procedure. Returns true iff procedure was activated just now. */
  private[proc] def activate() : Boolean = {
    if (process != null)
      return false
    process = action.start()
    true
  }



  /** Runs a process and returns next procedure to await. */
  private[proc] def roll() : Procedure = {
    val res = process.proceedTillNextProcedure()
    if (res == null)
      process = null
    res
  }



  /** Cleans up this procedure.*/
  private[proc] def cleanup() : Unit = {
    process = null
    if (onCleanup != null)
      onCleanup()
  }
}




/** Procedure manipulation object. */
object Procedure {
  /**
   * Compiles a specification into executable procedure.
   * <p>Procedure will be activated automatically when any of its dependencies
   * is activated. This dependency tracking supports automatic dependency
   * management (i.e. binding could be changed during action execution).
   * <p>If any of dependencies is active (evaluating right now) then this
   * procedure will be automatically executed during the update wave.
   * @param spec procedure specification.
   * @param binder dependency binding context.
   * @param onCleanup optional wave cleanup function. Called after all
   *   procedures in the transaction are executed.
   */
  def compile(
        spec : Specification,
        binder : Binder,
        onCleanup : () ⇒ Unit = null)
      : Procedure =
    new Procedure(spec, binder, onCleanup)



  /**
   * Creates a new action with manual activator.
   * @param onActivation action to invoke when procedure is activated
   *   (full procedure body)
   * @param onCleanup optional wave cleanup function.
   */
  def generator(
        onActivation : () ⇒ Unit,
        onCleanup : () ⇒ Unit = null)
      : (Activator, Procedure) = {
    val proc = new Procedure(Specification.forUnit {onActivation()}, null, onCleanup)
    (new Activator(proc), proc)
  }
}
