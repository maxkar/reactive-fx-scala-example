package ru.maxkar.fx

import java.awt.event.ActionEvent
import java.awt.event.ActionListener

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import scala.language.implicitConversions
import scala.collection.JavaConverters._

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



  /** Conversion between block and action handler. */
  implicit def actionHandler(handler : ActionEvent ⇒ Unit) : ActionListener =
    new ActionListener() {
      override def actionPerformed(e : ActionEvent) : Unit =
        handler(e)
    }
}
