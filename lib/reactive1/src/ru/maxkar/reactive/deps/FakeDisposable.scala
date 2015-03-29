package ru.maxkar.reactive.deps


/** Fake implementation of disposable. */
private[deps] final object FakeDisposable extends Disposable {
  override def dispose() : Unit = ()
}
