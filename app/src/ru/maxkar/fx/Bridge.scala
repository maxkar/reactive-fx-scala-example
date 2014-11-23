package ru.maxkar.fx

import javafx.event.Event
import javafx.event.EventHandler

import javafx.beans.value._

import javafx.collections.ObservableList
import javafx.collections.FXCollections

import ru.maxkar.lib.reactive.event.{Event ⇒ REvent}
import ru.maxkar.lib.reactive.event.Trigger
import ru.maxkar.lib.reactive.wave.Wave
import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

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


  private class Prop2TriggerBridge[T](trigger : Trigger)
      extends ChangeListener[T] {
    override def changed(
          v : ObservableValue[_ <: T],
          oldValue : T,
          newValue : T)
        : Unit =
      Wave.group(w ⇒ trigger.trigger(w))
  }


  /** Converts property into behaviour. */
  implicit def property2behaviour[T](
        prop : ObservableValue[T])(
        implicit ctx : BindContext)
      : Behaviour[T] = {
    val trigger = REvent.trigger(ctx.update)
    val listener = new Prop2TriggerBridge[T](trigger)

    prop.addListener(listener)
    ctx.lifespan.onDispose(() ⇒ prop.removeListener(listener))

    new Behaviour[T] {
      override def value() : T = prop.getValue()
      override val change = trigger.event
    }
  }



  implicit class ObservableSeqExt[T](val v : Seq[T]) extends AnyVal {
    def asFXList() : ObservableList[T] =
      FXCollections.observableList(v.toList.asJava)
  }



  private class Prop2SetterBridge[T](setter : T ⇒ Unit, guard : Behaviour[T])
      extends ChangeListener[T] {
    override def changed(
          v : ObservableValue[_ <: T],
          oldValue : T,
          newValue : T)
        : Unit =
      if (guard.value != newValue)
        setter(newValue)
  }



  /**
   * Binding for read-only properties with the setter.
   */
  def bindRO[T](
        prop : ObservableValue[T],
        psetter : T ⇒ Unit,
        value : Behaviour[T],
        setter : T ⇒ Unit)(
        implicit ctx : BindContext)
      : Unit = {
    val listener = new Prop2SetterBridge[T](setter, value)
    prop.addListener(listener)
    ctx.lifespan.onDispose(() ⇒ prop.removeListener(listener))

    value :< (v ⇒
      if (v != prop.getValue)
        psetter(v))
  }



  /**
   * Creates an unidirectional binding between FX model and
   * corresponding reactive value.
   * @param prop javaFX property model
   * @param value reactive value, source for the model.
   * @param setter value setter to invoke when FX model changes.
   */
 def bind[T](
      prop : WritableValue[T] with ObservableValue[T],
      value : Behaviour[T],
      setter : T ⇒ Unit)(
      implicit ctx : BindContext)
    : Unit =
   bindRO(prop, prop.setValue, value, setter)
}
