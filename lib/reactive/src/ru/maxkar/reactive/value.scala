package ru.maxkar.reactive

import ru.maxkar.reactive.wave.Participable
import ru.maxkar.reactive.value.event.Event

/**
 * Reactive definitions and operations package.
 * Provides a conveinence import mechanics by importing
 * ru.maxkar.reactive._ only.
 */
package object value {
  import scala.language.implicitConversions

  /**
   * Permanent binding between base and dependent values.
   */
  val permanentBind : BindContext =
    new BindContext(Lifespan.forever, Participable.DefaultParticipable)


  /**
   * Creates a new behaviour variable.
   * @param v initial value.
   */
  def variable[T](v : T) : Variable[T] = new Variable(v)



  /**
   * Convents value into behaviour constant.
   * @param v behaviour value.
   */
  def const[T](v : T) : Behaviour[T] = new Behaviour[T] {
    override def value() = v
    override val change = Event.constFalseEvent
  }


  implicit def behaviourOf[T](v : Variable[T]) : Behaviour[T] = v.behaviour
}
