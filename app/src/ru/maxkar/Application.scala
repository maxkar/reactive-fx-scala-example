package ru.maxkar

import java.io.File

import java.awt.image.BufferedImage
import java.awt.BorderLayout
import java.awt.FlowLayout

import java.awt.event.KeyListener
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.event.ActionEvent

import javax.swing.JComponent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JFileChooser
import javax.swing.JSplitPane
import javax.swing.JComboBox
import javax.swing.WindowConstants
import javax.swing.KeyStroke
import javax.swing.AbstractAction

import ru.maxkar.util._
import ru.maxkar.util.vfs._

import ru.maxkar.ui._
import ru.maxkar.ui.vfs._
import ru.maxkar.ui.image.ImageLoaderView

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import scala.collection.JavaConversions._


class Application(
      iohandler : AsyncExecutor,
      bro : FileWalker,
      fsRenderer : (BufferedImage, BufferedImage, BufferedImage, BufferedImage),
      shutdownHandler : () ⇒ Unit) {
  import Application._


  /** Application-wide lifespan. */
  private implicit val bindContext = permanentBind


  /** Zoom level presets. */
  private val zoomLevels = Seq(0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0)



  def start() : Unit = {
    var primaryStage = new JFrame("Image viewer")
    val root = new JPanel()
    root setLayout new BorderLayout()
    primaryStage setContentPane root

    val fc = new JFileChooser()

    val cnt = new JSplitPane()

    val curFile = variable[DirectoryEntry](null)
    val fsRender =
      FileWalkerView.make(bro,
        fsRenderer._1, fsRenderer._2, fsRenderer._3, fsRenderer._4)
    fsRender.requestFocus()

    root.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
      .put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "navfs")
    root.getActionMap.put("navfs", new AbstractAction() {
      override def actionPerformed(e : ActionEvent) : Unit =
        bro.open()
    })


    val file = bro.selection ≺ (item ⇒
      if (item == null)
        null
      else
        item match {
          case FileInfo.NestedItem(FileType.Image, e) if e.filestream ⇒
            e.backingFile()
          case _ ⇒ null
        }
      )

    val zoom = variable[Zoom](Zoom.Fixed(1.0))

    val imageui = ImageLoaderView.autoMake(iohandler, file, zoom)

    val opText = iohandler.operationCount ≺ (x ⇒ "IO ops: " + x)

    val zooms = Zoom.SmartFit +: Zoom.Fit +: zoomLevels.map(Zoom.Fixed(_))
    val zoomBox = Controls.combo[Zoom](zooms, zoom, zoom.set)

    val bottom = new JPanel()
    bottom setLayout new FlowLayout(FlowLayout.LEADING)
    bottom add Controls.label(opText)
    bottom add zoomBox
    bottom add Controls.label(zoomText _ ≻ imageui.effectiveZoom)

    root.add(bottom, BorderLayout.PAGE_END)
    cnt setLeftComponent fsRender
    cnt setRightComponent imageui.ui

    root.add(cnt, BorderLayout.CENTER)

    root.addKeyListener(new KeyAdapter() {
      override def keyPressed(e : KeyEvent) : Unit = {
        val newZoom = Zoom.zoomForShortcut(
          zoomLevels, imageui.effectiveZoom.value, zoom.value, e)
        newZoom match {
          case Some(x) ⇒
            zoom set x
            e.consume()
          case _ ⇒ ()
        }
      }})

    primaryStage addWindowListener new WindowAdapter() {
      override def windowClosing(e : WindowEvent) : Unit = {
        shutdownHandler()
        primaryStage.dispose()
      }
    }
    primaryStage setDefaultCloseOperation WindowConstants.DO_NOTHING_ON_CLOSE

    primaryStage.pack()
    primaryStage setVisible true
  }
}


object Application {
  /** Formats a zoom text. */
  private def zoomText(zoom : Option[Double]) : String =
    zoom match {
      case None ⇒ "--"
      case Some(x) ⇒ "%2.2f%%".format(x)
    }
}
