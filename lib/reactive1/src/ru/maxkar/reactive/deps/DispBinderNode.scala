package ru.maxkar.reactive.deps


/**
 * Node in a list of disposable binders.
 */
private[deps] class DispBinderNode(peer : Disposable)
    extends Disposable {

  /** Previous node in the list. */
  private[deps] var prev : DispBinderNode = null

  /** Next node in the list. */
  private[deps] var next : DispBinderNode = null



  override def dispose() : Unit = {
    if (prev == null)
      return

    next.prev = prev
    prev.next = next

    prev = null


    disposeInternal()
  }



  /** Internal disposer. Used by binder. */
  private[deps] def disposeInternal() : Unit =
    peer.dispose()
}
