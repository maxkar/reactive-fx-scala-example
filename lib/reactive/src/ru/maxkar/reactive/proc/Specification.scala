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
  def compile(binder : DownstreamDeps) : Action
}



/**
 * Specification factories and utilities.
 */
final object Specification {
  /** No-operation action. */
  val noop : Specification =
    new Specification {
      override def compile(binder : DownstreamDeps) : Action = Action.noop
    }



  /**
   * Creates a specification which awaits all the procedures. Also
   * registers dependencies on the given procedures.
   */
  def await(items : Procedure*) : Specification =
    if (items.isEmpty)
      noop
    else
      new Specification() {
        override def compile(binder : DownstreamDeps) : Action = {
          items.foreach(binder += _)
          Action.await(items : _*)
        }
      }



  /** Preforms a specification which executes an empty action. */
  def forUnit(block : ⇒ Unit) : Specification =
    new Specification {
      override def compile(binder : DownstreamDeps) : Action =
        Action.forUnit(block)
    }



  /** Creates an action which performs dynamic binding to an element.
   * That action registers an (updateable) activation dependency and updates
   * dependency during the evaluation.
   */
  def dynamicBindTo(block : ⇒ Procedure) : Specification =
    new Specification {
      override def compile(binder : DownstreamDeps) : Action =
        Action.dynamicBindTo(binder, block)
    }



  /**
   * Performs a "sequential" specification which executes all
   * specifications in order.
   */
  def seq(specs : Specification*) : Specification =
    specs.size match {
      case 0 ⇒ noop
      case 1 ⇒ specs.head
      case _ ⇒
        new Specification() {
          override def compile(binder : DownstreamDeps) : Action =
            Action.seq(specs.map(_.compile(binder)) : _*)
        }
    }
}
