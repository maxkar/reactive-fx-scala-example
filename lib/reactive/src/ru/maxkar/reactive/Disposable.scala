package ru.maxkar.reactive


/**
 * Disposable element (like subcontext or relationship between nodes).
 */
trait Disposable {
  /**
   * Disposes an item (node, dependency, etc...). Multiple calls to this method
   * do not have any additional effect.
   */
  def dispose() : Unit
}
