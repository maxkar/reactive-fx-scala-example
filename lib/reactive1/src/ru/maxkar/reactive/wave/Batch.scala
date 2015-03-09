package ru.maxkar.reactive.wave


/**
 * Operation grouping and processing interface. Groups all actions and runs
 * as few updates as possible.
 */
trait Batch {
  /**
   * Schedules a node's to be executed.
   * Action for each node is executed at most one for each update "cycle". So
   * adding the same node more times would not cause the same action to be
   * executed more that once.
   */
  def schedule(node : Node[_]) : Unit
}
