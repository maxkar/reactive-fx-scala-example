package ru.maxkar.ui.tween

import javax.swing.Timer

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.proc._
import ru.maxkar.reactive.value._

import ru.maxkar.ui.syntax._

/**
 * Linear tweener implementation.
 */
private[ui] final class Linear(
      duration : Int, interval : Int,
      trigger : Behaviour[Boolean],
      ctx : BindContext)
    extends Behaviour[Int] {

  /** Timer used by thin linear tweener. */
  private val timer = new Timer(interval, action { activator.activate })



  /** Action cycle start. */
  private var timerStart : Long = -1



  /** Current value. */
  private var current : Int = 0



  /** Flag indicating that this tweener changed its value. */
  private var changed = false



  /** Action activator and action procedure. */
  private var (activator, proc) =
    Procedure.compileWithActivator(
      Specification.seq(
        Specification.await(trigger.change.procedure),
        Specification.forUnit { updateState }
      ),
      ctx.binder,
      () ⇒ changed = false)



  /** Updates internal behaviour state. */
  private def updateState() : Unit = {
    if (trigger.change.value)
      updateOnTrigger()
    else
      updateOnTimer()
  }




  /** Updates this tweener on trigger change. */
  private def updateOnTrigger() : Unit =
    if (trigger.value)
      activate()
    else
      passivate()



  /** Updates this tweener on timer change. */
  private def updateOnTimer() : Unit = {
    if (timerStart < 0)
      return

    changed = true
    current = Math.max(current, System.currentTimeMillis - timerStart).toInt
    if (current >= duration) {
      current = duration
      timer.stop()
    }
  }



  /** Activates this tweener. */
  private def activate() : Unit = {
    if (timerStart > 0)
      return

    timerStart = System.currentTimeMillis
    timer.start()
  }



  /** Passivates this tweener. */
  private def passivate() : Unit = {
    if (timerStart < 0)
      return

    changed = true
    current = 0
    timerStart = -1
    timer.stop()
  }



  /** Initializes the tweener. */
  private def init() {
    timer setRepeats true
    timer setCoalesce true
    if (trigger.value)
      activate()
  }
  init()



  /* IMPLEMENTATION */
  override def value() : Int = current
  override val change = Signal(proc, changed)
}
