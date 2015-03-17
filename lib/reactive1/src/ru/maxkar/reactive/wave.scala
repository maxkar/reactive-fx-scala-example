package ru.maxkar.reactive

import scala.collection.mutable.ArrayBuffer


/** Wave processing type. */
package object wave {
  /** State of one node.
   * @param T note type.
   */
  private[wave] abstract sealed class State[+T]

  /** Node is ready. */
  private[wave] case object Ready extends State[Nothing]


  /** Node is preparing for boot. */
  private[wave] case class Boot(deps : ArrayBuffer[Pin[_]]) extends State[Nothing]


  /** Node is waiting for something. */
  private[wave] case class Waiting(deps : ArrayBuffer[Pin[_]], itr : iter.DepIterator[Pin[_]])
      extends State[Nothing]


  /** Node is ready to be resolved. */
  private[wave] case class Resolving(deps : ArrayBuffer[Pin[_]])  extends State[Nothing]


  /** Node completed its operation. */
  private[wave] case class Resolved[T](v : T) extends State[T]
}
