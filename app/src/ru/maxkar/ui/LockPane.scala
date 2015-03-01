package ru.maxkar.ui

import java.awt.BorderLayout
import java.awt.Color
import java.awt.Graphics
import javax.swing.JComponent
import javax.swing.Timer

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import syntax._

/**
 * Lock pane controller implementation.
 * <em>Warning</em>. This pane behaves correctly only as a glass pane.
 * It is not correct to use this pane in a layered panes. It does not
 * respect focus subsystem to that extent.
 */
private[ui] class LockPane extends JComponent {
  import LockPane._

  /** Current lock pane state. */
  private var state : State = Unlocked



  override def paintComponent(g : Graphics) : Unit = {
    state match {
      case Locked ⇒
        g.setColor(PANE_COLOR)
        g.fillRect(0, 0, getWidth, getHeight)
      case Locking(_, tick) if tick > 6 ⇒
        g.setColor(new Color(128, 128, 128, 10 * (tick - 6)))
        g.fillRect(0, 0, getWidth, getHeight)
      case _ ⇒ ()
    }
  }



  /** Tries to lock this pane. */
  private def startLocking() : Unit =
    state match {
      case Unlocked ⇒
        setVisible(true)
        requestFocusInWindow()
        val timer = new Timer(LOCK_TIME_MS / 14, onAction(increaseLocking()))
        timer.start()
        state = Locking(timer, 0)
      case _ ⇒ ()
    }



  /** Increases locking progress. */
  private def increaseLocking() : Unit =
    state match {
      case Locking(timer, 10) ⇒
        timer.stop()
        state = Locked
        repaint()
      case Locking(timer, x) ⇒
        state = Locking(timer, x + 1)
        repaint()
      case _ ⇒ throw new IllegalStateException("Bad locking state " + state)
    }



  /** Unlocks this pane. */
  private def unlock() : Unit =
    state match {
      case Locked ⇒
        setVisible(false)
        state = Unlocked
      case Locking(timer, _) ⇒
        timer.stop()
        setVisible(false)
        state = Unlocked
      case Unlocked ⇒ ()
    }



  /** Updates "locked" state. */
  private def setLocked(locked : Boolean) : Unit =
    if (locked)
      startLocking()
    else
      unlock()
}




/**
 * Lock pane object companion.
 */
private[ui] object LockPane {

  /** Color for the lock pane. */
  private var PANE_COLOR = new Color(128, 128, 128, 84)

  /** Delay to complete lock. */
  private var LOCK_TIME_MS = 350



  /** Lock pane state definition. */
  private abstract sealed class State { }



  /** Unlocked state for this glass pane. */
  private case object Unlocked extends State
  /** Completely locked state for the glass pane. */
  private case object Locked extends State
  /** "Locking operation is in progress" state. */
  private case class Locking(timer : Timer, step : Int) extends State



  /** Creates a "lock UI" pane. */
  private[ui] def create(
        locked : Behaviour[Boolean])(
        implicit ctx : BindContext)
      : JComponent = {
    val res = new LockPane()
    res setOpaque false
    res setFocusable true
    res setFocusCycleRoot true
    res addMouseListener new java.awt.event.MouseAdapter(){}

    /* Swing is not declarative enough, so this workaround.
     * We have to set visibility after component is added into
     * glass pane. */
    nextTick(locked ≺ res.setLocked)

    res
  }
}
