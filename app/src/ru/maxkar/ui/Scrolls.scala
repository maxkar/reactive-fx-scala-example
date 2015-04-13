package ru.maxkar.ui

import java.awt.Point
import java.awt.Dimension
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

import javax.swing.JScrollPane
import javax.swing.JViewport

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


/**
 * Scroll pane utils.
 */
object Scrolls {
  /** Enables viewport scrolling by mouse drag gesture. */
  def scrollByDrag(vp : JViewport) : Unit = {
    var basePoint : Point = null

    vp.addMouseMotionListener(new MouseAdapter() {
      override def mouseDragged(e : MouseEvent) : Unit = {
        val cp = e.getPoint()
        val dx = cp.x - basePoint.x
        val dy = cp.y - basePoint.y

        val extent = vp.getExtentSize()
        val viewSize = vp.getViewSize()
        val msx = Math.max(0, viewSize.width - extent.width)
        val msy = Math.max(0, viewSize.height - extent.height)

        val vpos = vp.getViewPosition()

        vp.setViewPosition(new Point(
          bound(vpos.x - dx, 0, msx),
          bound(vpos.y - dy, 0, msy)))

        basePoint = cp
      }
    })

    vp.addMouseListener(new MouseAdapter() {
      override def mousePressed(e : MouseEvent) : Unit =
        basePoint = e.getPoint()
    })
  }



  /**
   * Returns viewport size as a behaviour.
   */
  def viewportSize(pane : JScrollPane) : Behaviour[Dimension] = {
    val res = variable(pane.getViewport().getExtentSize())

    pane addComponentListener new ComponentAdapter() {
      override def componentResized(e : ComponentEvent) : Unit =
        res set pane.getViewport().getExtentSize()
    }

    res
  }



  /**
   * Bounds int value between two items.
   */
  private def bound(x : Int, min : Int, max : Int) : Int =
    if (x < min)
      min
    else if (x > max)
      max
    else
      x
}
