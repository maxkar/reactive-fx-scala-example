package ru.maxkar.reactive.deps

/**
 * Iterator over dependency list items.
 * @param nextItem next (current) item in the iterator.
 */
private[deps] final class DLIterator[T]
      (private var nextItem : DLDependency[T])
    extends Iterator[T]{

  advance()

  /** Advanced over the list to "next item to return". */
  private def advance() : Unit = {
    do {
      nextItem = nextItem.next
    } while (nextItem.prev == null)


    /* Last dummy element should not be returned. */
    if (nextItem.next == null)
      nextItem = null
  }



  override def hasNext = nextItem != null



  override def next() : T = {
    if (nextItem.next == null)
      throw new IllegalStateException("No more elements in the iterator")
    val res = nextItem.node
    advance()
    res
  }
}
