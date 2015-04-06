package ru.maxkar.reactive.deps

import ru.maxkar.reactive.Disposable

/**
 * Fake binder implementation.
 */
private[deps] final object FakeBinder extends Binder {
  override def bind[T](list : DependencyList[T], item : T) : Disposable =
    FakeDisposable

  override def sub() : (Binder, Disposable) = (this, FakeDisposable)

}
