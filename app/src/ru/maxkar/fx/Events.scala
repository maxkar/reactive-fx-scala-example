package ru.maxkar.fx

import javafx.scene.input.KeyEvent

/**
 * Event utilities.
 */
object Events {
  def modifierDown(evt : KeyEvent) : Boolean =
    evt.isAltDown() ||
    evt.isControlDown() ||
    evt.isMetaDown() ||
    evt.isShiftDown() ||
    evt.isShortcutDown()
}
