package ru.maxkar.reactive.deps

import ru.maxkar.reactive.Disposable


/** Fake implementation of disposable. */
private[deps] final object FakeDisposable extends Disposable {
  override def dispose() : Unit = ()
}
