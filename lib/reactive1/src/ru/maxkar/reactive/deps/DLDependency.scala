package ru.maxkar.reactive.deps

import ru.maxkar.reactive.Disposable

/**
 * Dependency in a dependency list.
 * Efficiently it is a linked list node which could be removed from
 * the list.
 * @param T node type.
 * @param node element in this node.
 */
private[deps] final class DLDependency[T](
      private[deps] val node : T)
    extends Disposable {

  /** Previous node. */
  private[deps] var prev : DLDependency[T] = null

  /** Next node. */
  private[deps] var next : DLDependency[T] = null



  override def dispose() : Unit = {
    if (prev == null)
      return

    prev.next = next
    next.prev = prev

    prev = null
    /* Do not override next node to allow iterators pointing to this node
     * to return into the list even after this item was disposed/deleted.
     */
  }
}
