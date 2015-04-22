package ru.maxkar.ui

import javax.swing._
import javax.swing.event._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


/** Input UI factory. */
private[ui] final object InputUI {

  /**
   * Creates a new text input using behaviour and change callback.
   */
  def input(
        value : Behaviour[String],
        cb : String ⇒ Unit)(
        implicit ctx : BindContext)
      : JTextField = {
    val res = new JTextField()
    var rupdate = false
    var tupdate = false

    val sr = new Runnable() {
      override def run() : Unit = {
        rupdate = true
        try {
          res.setText(value.value)
        } finally {
          rupdate = false
        }
      }
    }


    value ≺ (v ⇒
      if (!tupdate)
        sr.run()
    )


    res.getDocument() addDocumentListener new DocumentListener() {
      def changedUpdate(e : DocumentEvent) : Unit = sync()
      def insertUpdate(e : DocumentEvent) : Unit = sync()
      def removeUpdate(e : DocumentEvent) : Unit = sync()

      private def sync() : Unit = {
        if (rupdate)
          return
        tupdate = true
        try {
          cb(res.getText())
          /* Resync to model. */
          if (value.value != res.getText())
            SwingUtilities invokeLater sr
        } finally {
          tupdate = false
        }
      }
    }

    res
  }
}
