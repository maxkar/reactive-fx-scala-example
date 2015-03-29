package ru.maxkar.reactive.deps


/**
 * Mutable list used to track dependencies between object.
 * This list supports easy additions and removals (unbinds) of elements.
 * Operation time:
 * <ul>
 *   <li>Insertion - constant time
 *   <li>Deletion - constant time
 * </ul>
 * @param T element type.
 */
final class DependencyList[T:Manifest] {
  /** First list element. */
  private var head = new DLDependency[T](null.asInstanceOf[T])

  /** Last list element. */
  private var tail = new DLDependency[T](null.asInstanceOf[T])

  head.next = tail
  tail.prev = head



  /** Adds an item into this dependency list.
   * @param item item to add into this list.
   * @return destructor of the binding. Disposing returned item will result
   * in removing association between <code>this</code> and <code>item</code>.
   */
  private[deps] def += (item : T) : Disposable = {
    val res = new DLDependency[T](item)

    res.prev = tail.prev
    res.prev.next = res

    res.next = tail
    tail.prev = res

    res
  }



  /** Creates an iterator over content of this list. */
  private[reactive] def iterator : Iterator[T] = new DLIterator[T](head)
}
