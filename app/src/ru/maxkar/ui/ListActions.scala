package ru.maxkar.ui


/** List actions specification and implementation. */
abstract class ListActions private[ui]() {
  /** Navigates one page down. */
  def pageDown() : Unit


  /** Navigates one page up. */
  def pageUp() : Unit


  /** Switches to a next item. */
  def nextItem() : Unit


  /** Switches to a prev item. */
  def prevItem() : Unit
}
