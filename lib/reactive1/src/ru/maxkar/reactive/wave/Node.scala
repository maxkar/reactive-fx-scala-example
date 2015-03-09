package ru.maxkar.reactive.wave



/**
 * One node in the wave.
 * Neither of iterators MUST NOT mutate model or model dependencies. Only
 * actions could mutate/update model and generate results.
 * @param T type of an internal node state.
 */
trait Node[T] {
  /** Returns dependencies (nodes which have to be evaluated before
   * the current one. It is guaranteed that "current" node is resolved
   * before iterator function is called again. However, this factory
   * COULD be called multiple times during an update. This is the reason
   * to have that iterator functions idempotent.
   */
  def deps() : NodeIterator


  /**
   * Return iterator over "upstream" nodes (nodes which could update
   * after this node updated, as managed by dependency manager).
   */
  def upstream() : NodeIterator



  /**
   * Performs a node action and returns value of that action.
   */
  def action(w : Wave) : T



  /**
   * Default value used when node is not in a wave but requested.
   */
  val defaultValue : T



  /**
   * Merges event generated in the wave into one composite event.
   * @param t1 earlier event.
   * @param t2 later event.
   */
  def merge(t1 : T, t2 : T) : T
}

