package ru.maxkar.util

import scala.language.implicitConversions


/**
 * Different utilities for java.util.Runnable and similar stuff.
 */
object Runnables {
  /** Converts function to runnable. */
  implicit def fn2runnable[T](fn : () ⇒ T) : Runnable =
    new Runnable() {
      override def run() : Unit = fn()
    }


  /** Block-like syntax for runnable. */
  def run[T](block : ⇒ T) : Runnable =
    new Runnable() {
      override def run() : Unit = block
    }
}
