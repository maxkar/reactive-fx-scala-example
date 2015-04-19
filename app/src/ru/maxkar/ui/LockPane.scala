package ru.maxkar.ui

import java.awt.Color
import java.awt.Graphics
import javax.swing.JComponent

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import syntax._

/**
 * Lock pane controller implementation.
 * <em>Warning</em>. This pane behaves correctly only as a glass pane.
 * It is not correct to use this pane in a layered panes. It does not
 * respect focus subsystem to that extent.
 */
private[ui] class LockPane(fadeRate : Behaviour[Int])  extends JComponent {
  /** Sets locked state. */
  private def setLocked(locked : Boolean) : Unit = {
    setVisible(locked)
    if (locked)
      requestFocusInWindow()
  }



  override def paintComponent(g : Graphics) : Unit = {
    g.setColor(new Color(128, 128, 128, fadeRate.value))
    g.fillRect(0, 0, getWidth, getHeight)
  }
}




/**
 * Lock pane object companion.
 */
private[ui] object LockPane {
  /** Delay to complete lock. */
  private var LOCK_TIME_MS = 350

  /** Small lock timeot. */
  private var GRAYOUT_MS = 100


  /** Calculates fade rate. */
  private def getFadeRate(time : Int) : Int =
      if (time < GRAYOUT_MS)
        0
      else
        80 * (time - GRAYOUT_MS) / (LOCK_TIME_MS - GRAYOUT_MS)



  /** Creates a "lock UI" pane. */
  private[ui] def create(
        locked : Behaviour[Boolean])(
        implicit ctx : BindContext)
      : JComponent = {
    val fade = Tween.linear(LOCK_TIME_MS, LOCK_TIME_MS / 14, locked) ≺ getFadeRate
    val res = new LockPane(fade)
    fade ≺ (_ ⇒ res.repaint())
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
