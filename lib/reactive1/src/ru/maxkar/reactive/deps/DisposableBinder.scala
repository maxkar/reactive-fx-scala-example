package ru.maxkar.reactive.deps

import ru.maxkar.reactive.Disposable

/**
 * Binder for temporary relationships. Destroys all relationships
 * when this binder is destroyed.
 */
private[deps] final class DisposableBinder extends Binder {
  /** Peer dependencies registered here. */
  private var items = new DependencyList[Disposable]



  /** Destructor/cleaner for the binder. */
  private[deps] val disposer = new Disposable() {
    override def dispose() : Unit = DisposableBinder.this.dispose()
  }



  override def bind[T](list : DependencyList[T], item : T) : Disposable =
    if (items == null)
      FakeDisposable
    else {
      val base = list += item
      val reg = items += base
      new PairDisposer(reg, base)
    }



  override def sub() : (Binder, Disposable) =
    if (items == null)
      (FakeBinder, FakeDisposable)
    else {
      val binder = new DisposableBinder()
      val reg = items += binder.disposer
      (binder, new PairDisposer(reg, binder.disposer))
    }



  /** Disposes an item. */
  private[deps] def dispose() : Unit = {
    if (items == null)
      return

    val itr = items.iterator
    items = null

    while(itr.hasNext)
      itr.next.dispose()
  }
}
