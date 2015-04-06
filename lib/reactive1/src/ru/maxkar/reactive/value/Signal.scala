package ru.maxkar.reactive.value

import ru.maxkar.reactive.proc.Procedure

/**
 * Signal implementation. Each signal contains its activation
 * procedure and its value (retained during update). Signals
 * usually returns to default (zero) value after wave completion.
 * @param T signal value type
 * @param procedure activation/execution procedure. Other processors could
 *   use this procedure to install its own dependencies.
 */
final class Signal[T] private(
      val procedure : Procedure,
      valueImpl : ⇒ T) {
  /**
   * <p>Returns current value of this signal. You are not allowed to call
   *   this method when <code>procedure</code> is active. You have to await
   *   procedure in the execution process and then access this method.
   * <p>This method usually returns "default" value if signal is not involved
   *   in some update.
   *
   * @return value of this signal at the current moment.
   * @throws IllegalStateException if <code>procedure</code> is active at
   * this moment.
   */
  def value() : T = {
    if (procedure.isActive())
      throw new IllegalStateException("Could not access signal until its procedure is complete")
    valueImpl
  }
}



/**
 * Signal companion object.
 */
object Signal {
  /**
   * Creates a new signal node using provided procedure and
   * value retriever.
   */
  def apply[T](procedure : Procedure, valueImpl : ⇒ T) : Signal[T] =
    new Signal[T](procedure, valueImpl)
}
