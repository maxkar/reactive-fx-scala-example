package ru.maxkar.ui

import actions.ActionMap
import actions.ActionSpec
import keys.Keymap

import java.awt.event.ActionEvent

import java.awt.Dimension
import javax.swing.Action
import javax.swing.AbstractAction
import javax.swing.JComponent

/**
 * Different syntax enhancements for the UI package.
 */
object syntax {
  /**
   * Magic component operations.
   */
  implicit class ComponentOps[T <: JComponent](val comp : T) extends AnyVal {
    /** Sets a preferred component size and returns that component. */
    def ~ (preferredSize : (Int, Int)) : T = {
      comp setPreferredSize new Dimension(preferredSize._1, preferredSize._2)
      comp
    }


    /**
     * Creates a new component action extender.
     */
    def actions() : ActionMap = new ActionMap(comp.getActionMap())



    /** Returns a key bindings when this component is focused. */
    def keysWhenFocused() : Keymap =
      new Keymap(comp.getInputMap(JComponent.WHEN_FOCUSED))


    /** Returns a key bindings when this component is focused or
     * is ancestor of a focused component.
     */
    def keysWhenFocusedAncestor() : Keymap =
      new Keymap(comp.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT))
  }



  /** Magic string operations. */
  implicit class StrMagicOps(val str : String) extends AnyVal {
    /** Creates an action for the name. */
    def :-> (handler : ⇒ Unit) : ActionSpec =
      new ActionSpec(str, action(handler))
  }



  /**
   * Creates an action from block.
   */
  def action(x : ⇒ Unit) : Action =
    new AbstractAction() {
      override def actionPerformed(e : ActionEvent) : Unit = x
    }
}
