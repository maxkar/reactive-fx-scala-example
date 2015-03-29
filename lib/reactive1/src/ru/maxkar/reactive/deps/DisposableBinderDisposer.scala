package ru.maxkar.reactive.deps


/** Disposer for the disposable binder. */
private[deps] final class DisposableBinderDisposer(
      peer : DisposableBinder)
    extends Disposable {

  /** Disposes a disposable binder. */
  override def dispose() : Unit = peer.dispose()
}
