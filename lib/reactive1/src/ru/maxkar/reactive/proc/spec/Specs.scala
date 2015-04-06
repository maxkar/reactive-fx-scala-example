package ru.maxkar.reactive.proc.spec

import ru.maxkar.reactive.proc._


/**
 * Specification manipulation and generation.
 */
object Specs {
  /** Empty (simple) specification. */
  private val noop = exec {}



  /** Executes an explicit action without additional dependencies. */
  def exec(action : ⇒ Unit) : Specification =
    new Specification() {
      def compile(binder : DownstreamDeps) : Action =
        new Action() {
          override def start() : Process =
            new Process() {
              override def proceedTillNextProcedure() : Procedure = {
                action
                null
              }
            }
        }
    }



  /** Executes a sequence of steps. */
  private def sequenceImpl(specs : Seq[Specification]) : Specification =
    new Specification() {
      def compile(binder : DownstreamDeps) : Action = {
        val peerActions = specs.map(x ⇒ x.compile(binder))
        act { new SeqProcess(peerActions) }
      }
    }



  /** Creates a sequence of actions. */
  def sequence(specs : Specification*) : Specification =
    specs.length match {
      case 0 ⇒ exec {}
      case 1 ⇒ specs.head
      case _ ⇒ sequenceImpl(specs)
    }



  /** Awaits until all procedures are complete. */
  def await(procs : Procedure*) : Specification =
    new Specification() {
      def compile(binder : DownstreamDeps) : Action = {
        procs.foreach(x ⇒ binder += x)
        act { new AwaitProcess(procs) }
      }
    }



  /** Creates an "await" process for the dynamic action. */
  def awaitDynamic(dep : () ⇒ Procedure) : Specification =
    new Specification() {
      def compile(binder : DownstreamDeps) : Action =
        new DynBindAction(binder, dep)
    }



  /** Creates an action. */
  private def act(fn : ⇒ Process) : Action =
    new Action() {
      override def start() : Process = fn
    }
}
