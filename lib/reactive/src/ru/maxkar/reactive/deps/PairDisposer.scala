package ru.maxkar.reactive.deps

import ru.maxkar.reactive.Disposable

/**
 * Disposer which disposes two items.
 */
private[deps] final class PairDisposer(
      d1 : Disposable, d2 : Disposable)
    extends Disposable {

  override def dispose() : Unit = {
    d1.dispose()
    d2.dispose()
  }
}
