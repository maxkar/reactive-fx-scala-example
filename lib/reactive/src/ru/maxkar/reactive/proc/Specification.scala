package ru.maxkar.reactive.proc


/**
 * Specification factories and utilities.
 */
final object Specification {
  /** No-operation action. */
  val noop : Specification = (binder) ⇒ Action.noop



  /**
   * Creates a specification which awaits all the procedures. Also
   * registers dependencies on the given procedures.
   */
  def await(items : Procedure*) : Specification =
    if (items.isEmpty)
      noop
    else
      (binder) ⇒ {
        items.foreach(binder += _)
        Action.await(items : _*)
      }



  /** Preforms a specification which executes an empty action. */
  def forUnit(block : ⇒ Unit) : Specification =
    binder ⇒ Action.forUnit(block)



  /** Creates an action which performs dynamic binding to an element.
   * That action registers an (updateable) activation dependency and updates
   * dependency during the evaluation.
   */
  def dynamicBindTo(block : ⇒ Procedure) : Specification =
    (binder) ⇒ Action.dynamicBindTo(binder, block)



  /**
   * Performs a "sequential" specification which executes all
   * specifications in order.
   */
  def seq(specs : Specification*) : Specification =
    specs.size match {
      case 0 ⇒ noop
      case 1 ⇒ specs.head
      case _ ⇒
        (binder) ⇒ Action.seq(specs.map(s ⇒ s(binder)) : _*)
    }
}
