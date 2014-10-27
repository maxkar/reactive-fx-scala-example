package ru.maxkar.fx

import javafx.event.Event
import javafx.event.EventHandler

import scala.language.implicitConversions

/** Platform bridge. Implicits, conversions, etc... */
object Bridge {
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


  /** Event handler converter. */
  implicit def fn2handler[T <: Event](fn : T ⇒ Unit) : EventHandler[T] =
    new EventHandler[T] {
      override def handle(evt : T) : Unit = fn(evt)
    }


  /** Another event handler converter. */
  implicit def fno2handler[T <: Event](fn : () ⇒ Unit) : EventHandler[T] =
    new EventHandler[T] {
      override def handle(evt : T) : Unit = fn()
    }


  /** Block event handler converter. */
  implicit def evt[T <: Event](block : ⇒ Unit) : EventHandler[T] =
    new EventHandler[T] {
      override def handle(evt : T) : Unit = block
    }
}
