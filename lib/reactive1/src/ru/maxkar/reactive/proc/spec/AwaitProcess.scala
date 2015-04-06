package ru.maxkar.reactive.proc.spec

import ru.maxkar.reactive.proc._


/**
 * Process to wait for peer completion.
 */
final class AwaitProcess(items : Iterable[Procedure])
    extends Process {

  /** Active iterator. */
  private val itr = items.iterator

  override def proceedTillNextProcedure() : Procedure =
    if (itr.hasNext)
      itr.next
    else
      null
}
