package ru.maxkar.reactive.value


import ru.maxkar.fun._


/** Behaviour/binding lifetime.*/
abstract class Lifetime private[value]() {
  /** Adds a "dispose" action into the lifetime object. */
  protected def onDispose(action : () ⇒ Unit) : Unit
}
