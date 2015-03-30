package ru.maxkar.reactive.proc


/**
 * Abstract definition of an action. Used by procedure compiler to
 * create actual actions bound to the procedure.
 * <p>Users are encouraged to use standard specification constructors
 *   provided by the library (basic, collection, etc...) because
 *   specification API could change in future versions.
 */
trait Specification {
  /**
   * Compiles a specification using a provided activation binder.
   * Activation binder could be used only during compile time
   * (for static dependencies). It could be stored inside action and
   * used during runtime (to update/reconfigure dependency graph).
   *
   * <p>Binder should not be called outside of compilation (i.e.
   *  during current call to <code>compile</code>) and action (i.e. call
   *  to Action.start() or Process.processTillNextProcedure).
   * @param binder class used to instantiate dependencies between
   *  new procedure and existing ones.
   * @return compiled representation of action defined by this specification.
   */
  def compile(binder : DependencyBinder) : Action
}
