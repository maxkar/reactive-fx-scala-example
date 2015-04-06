package ru.maxkar.reactive

import ru.maxkar.reactive.deps.PermanentBinder
import ru.maxkar.reactive.proc.Procedure
import ru.maxkar.reactive.proc.spec.Specs


/** Value-related reactive definitions. */
package object value {
  import scala.language.implicitConversions

  /**
   * Permanent binding between base and dependent values.
   */
  val permanentBind : BindContext = new BindContext(PermanentBinder)


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
    override val change = Signal(Procedure.generator(() ⇒ ())._2, false)
  }
}
