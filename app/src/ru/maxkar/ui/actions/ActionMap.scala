package ru.maxkar.ui.actions

/**
 * Action map wrapper for the component.
 */
final class ActionMap(peer : javax.swing.ActionMap) {
  /**
   * Adds a single element to action map.
   */
  def add(spec : ActionSpec) : ActionMap = {
    peer.put(spec.ref, spec.action)
    this
  }



  /** Adds all specifications into this map. */
  def addAll(specs : ActionSpec*) : ActionMap = this ++= specs



  /** Adds all actions from the list. */
  def ++=(specs : Seq[ActionSpec]) : ActionMap = {
    specs.foreach(s ⇒ peer.put(s.ref, s.action))
    this
  }
}
