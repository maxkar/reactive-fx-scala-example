package ru.maxkar.fx


import java.awt.BorderLayout
import javax.swing.JComponent
import javax.swing.JPanel

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

/** Javafx node operations. */
object Nodes {

  /** Creates a node which have a content of the target node. */
  def contentOf(
        ui : Behaviour[JComponent])(
        implicit ctx : BindContext)
      : JComponent = {

    var last : JComponent = null

    val res = new JPanel()
    res setLayout new BorderLayout()

    def update(nc : JComponent) : Unit = {
      if (last != null)
        res.remove(last)
      last = nc
      if (nc != null)
        res.add(nc, BorderLayout.CENTER)
      res.revalidate()
      res.repaint()
    }

    ui ≺ update

    res
  }
}
