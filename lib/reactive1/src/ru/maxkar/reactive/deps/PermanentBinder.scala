package ru.maxkar.reactive.deps


/**
 * Binder which establishes "permanent" relationship between
 * lists and items. Permanent mean that dependency could be removed only
 * by calling its destructor.
 */
final object PermanentBinder extends Binder {
  override def bind[T](list : DependencyList[T], item : T) : Disposable =
    list += item


  override def sub() : (Binder, Disposable) = {
    val binder = new DisposableBinder()
    (binder, new DisposableBinderDisposer(binder))
  }
}
