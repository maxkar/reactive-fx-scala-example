package ru.maxkar.reactive


import scala.collection.mutable.ArrayBuffer


/** Update wave propagation and manipulation package. */
package object wave {
  /** Stateful function which returns dependency nodes or
   * <code>null</code> when last node was reached.
   * This function must not change "model" object, only iterator itself.
   */
  type NodeIterator = () ⇒ Node[_]



  /** Function used to create a new node iterator. */
  type NodeIterable = () ⇒ NodeIterator



  /**
   * Dependency between nodes.
   * @param ref referenced (dependent) node.
   */
  private[wave] final class ActiveDep(val node : Node[_]) {
    /** Flag indicating that this dependency is active. */
    var active = true
  }



  /** Node resolution state. */
  private[wave] sealed abstract class ResolveState[+T]



  /** Node is "booting" and waiting for its first access. */
  private[wave] case object Boot extends ResolveState[Nothing]
  /** Node was resolved in this wave. */
  private[wave] case class Resolved[+T](value : T) extends ResolveState[T]
  /** Node is undergoing an update.
   * @param curDep current (unsatisfied) dependency.
   * @param iterator dependency iterator.
   * @param deps dependencies registered on this node.
   */
  private[wave] case class Updating(
        var curDep : ActiveDep,
        iterator : NodeIterator,
        deps : ArrayBuffer[ActiveDep])
      extends ResolveState[Nothing]

  /** Node is undergoing completion. */
  private[wave] case class Completing(deps : ArrayBuffer[ActiveDep]) extends ResolveState[Nothing]



  /** Status of node update in parent waves. */
  private[wave] sealed abstract class RefreshStatus
  /** Update should be performed in a separate sub-wave. */
  private[wave] case object NeedSubwave extends RefreshStatus
  /** Value was not calculated and will be refreshed. */
  private[wave] case object RefreshScheduled extends RefreshStatus
  /** Value was not found in waves. */
  private[wave] case object NotFound extends RefreshStatus
}
