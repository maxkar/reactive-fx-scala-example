package ru.maxkar.ui

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/**
 * Different tweeners (factories for values changing over time).
 * Most tweeners takes in "activation" behaviour (simple boolean flag)
 * and starts its action when behaviour value it <code>true</code>.
 * Finishes its action when value is <code>false</code>.
 * <p>Actual tweener change and "finishing action" depends on an
 * actual tweener used. Tweener could provide "immediate" reset
 * or gradual return to initial value. Tweeners could be repeatable,
 * "back-and-forth" or "start-to-end". All this is also defined by the
 * concrete tweener used.
 */
object Tween {
  /**
   * Creates simple start-to-end time-based tweener.
   * Value is set to 0 when trigger is not active. It changes
   * to <code>duration</code> during <code>duration</code> milliseconds
   * with update interval of <code>interval</code> millis. Value stays
   * at it maximal value after that. Changes to 0 immediately when trigger
   * is set to <code>false</code>.
   * @param duration tweener total duration.
   * @param interval tweener update interval (suggested value).
   * @param trigger behaviour.
   * @param ctx tweener bind context.
   */
  def linear(
        duration : Int, interval : Int,
        trigger : Behaviour[Boolean])(
        implicit ctx : BindContext)
      : Behaviour[Int] = {
    if (duration <= 0)
      throw new IllegalArgumentException("Trigger duration must be positive but is " + duration)
    if (interval <= 0)
      throw new IllegalArgumentException("Interval value must be positive but is " + interval)
    new tween.Linear(duration, interval, trigger, ctx)
  }
}
