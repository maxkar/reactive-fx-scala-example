package ru.maxkar.reactive



/**
 * Iterators definition and manipulation.
 */
package object iter {
  /**
   * Dependency iterator. Returns each dependency upon each
   * call. It is guaranteed that last returned dependency is
   * resolved before next call to the iterator. Returns <code>None</code>
   * to indicate that there are no remaining dependencies.
   */
  type DepIterator[+T] = () ⇒ Option[T]


  /**
   * Iterable for dependencies.
   */
  type DepIterable[+T] = () ⇒ DepIterator[T]



  /** Empty iterator. */
  private val depEmpty : DepIterator[Nothing] = () ⇒ None


  /** Converts iterator into dependency iterator. */
  def iter2dep[T](iter : Iterator[T]) : DepIterator[T] =
    () ⇒ if (iter.hasNext) Some(iter.next) else None



  /** Single item iterator. */
  def depSingle[T](item : T) : DepIterator[T] = {
    var returned = false
    () ⇒
      if (returned)
        None
      else {
        returned = true
        Some(item)
      }
  }



  /** An empty iterator. */
  val emptyIter : DepIterable[Nothing] = () ⇒ depEmpty



  /** Creates a single-item iterator. */
  def singleIter[T](item : T) : DepIterable[T] =
    () ⇒ depSingle(item)



  /** Creates an items iterator. */
  def itemsIter[T](items : T*) : DepIterable[T] =
    items.size match {
      case 0 ⇒ emptyIter
      case 1 ⇒ singleIter(items.head)
      case _ ⇒ () ⇒ iter2dep(items.iterator)
    }



  /** Chains multiple iterables. */
  def seqIters[T](iters : DepIterable[T]*) : DepIterable[T] =
    iters.size match {
      case 0 ⇒ emptyIter
      case 1 ⇒ iters.head
      case _ ⇒ () ⇒ new IterableIter(iters).next
    }
}
