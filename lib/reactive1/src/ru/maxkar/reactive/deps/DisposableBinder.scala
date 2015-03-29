package ru.maxkar.reactive.deps


/**
 * Binder for temporary relationships. Destroys all relationships
 * when this binder is destroyed.
 */
private[deps] final class DisposableBinder extends Binder {
  /** Dummy header item. */
  private var head = new DispBinderNode(null)


  /** Dummy tail item. */
  private var tail = new DispBinderNode(null)



  override def bind[T](list : DependencyList[T], item : T) : Disposable =
    if (head == null)
      FakeDisposable
    else
      insert(list += item)



  override def sub() : (Binder, Disposable) = {
    if (head == null)
      return (FakeBinder, FakeDisposable)

    val binder = new DisposableBinder()
    (binder, insert(new DisposableBinderDisposer(binder)))
  }




  /** Insterts a disposable into this list. */
  private def insert(item : Disposable) : Disposable = {
    val res = new DispBinderNode(item)
    res.prev = tail.prev
    res.prev.next = res

    tail.prev = res
    res.next = tail

    res
  }




  /** Disposes an item. */
  private[deps] def dispose() : Unit = {
    if (head == null)
      return

    var cur = head.next
    head = null
    tail = null

    while (cur.next != null) {
      cur.disposeInternal()
      cur = cur.next
    }
  }
}
