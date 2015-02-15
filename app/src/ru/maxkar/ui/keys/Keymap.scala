package ru.maxkar.ui.keys

import javax.swing.KeyStroke

/**
 * Key mapping for the component.
 */
final class Keymap(peer : javax.swing.InputMap) {
  /** Adds all mappings from key to action. */
  def ++=(keys : Seq[(String, String)]) : Keymap = {
    keys.foreach(k ⇒ peer.put(KeyStroke.getKeyStroke(k._1), k._2))
    this
  }


  /** Adds all mappings from key to action. */
  def addAll(keys : (String, String)*) : Keymap = this ++= keys
}
