package ru.maxkar

import java.awt.image.BufferedImage

import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

import javax.swing.JComponent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.WindowConstants
import javax.swing.SwingUtilities

import ru.maxkar.util._
import ru.maxkar.util.vfs._
import ru.maxkar.async._

import ru.maxkar.ui._
import ru.maxkar.ui.syntax._
import ru.maxkar.ui.vfs._
import ru.maxkar.ui.image.ImageLoaderView

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


class Application(
      iohandler : CountingExecutor,
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

    val curFile = variable[DirectoryEntry](null)
    val (fsRender, validSelection) =
      FileWalkerView.make(bro,
        fsRenderer._1, fsRenderer._2, fsRenderer._3, fsRenderer._4)


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

    val (imageui, effectiveZoom) = ImageLoaderView.autoMake(iohandler, file, zoom)
    val zooms = Zoom.SmartFit +: Zoom.Fit +: zoomLevels.map(Zoom.Fixed(_))

    val root = Layouts.border(
      center = Controls.hsplit(
        fsRender ~ (250, 300),
        imageui ~ (500, 300)
      ),
      south = Layouts.leftToRightFlow(
        Controls.label(iohandler.operationCount ≺ (x ⇒ "IO ops: " + x)),
        Controls.combo[Zoom](zooms, zoom, zoom.set),
        Controls.label(effectiveZoom ≺ zoomText)
      ))

    root.actions ++=
      FileWalkerView.navActionsFor("navfs:", validSelection, bro) ++=
      Zoom.zoomActionsFor("zoom:", zoomLevels, effectiveZoom, zoom, zoom.set)
    root.keysWhenFocusedAncestor ++=
      FileWalkerView.defaultKeyBindings("navfs:") ++=
      Zoom.defaultKeyBindings("zoom:")

    primaryStage setContentPane root

    primaryStage addWindowListener new WindowAdapter() {
      override def windowClosing(e : WindowEvent) : Unit = {
        shutdownHandler()
        primaryStage.dispose()
      }
    }
    primaryStage setDefaultCloseOperation WindowConstants.DO_NOTHING_ON_CLOSE

    primaryStage setGlassPane Controls.lockPane(bro.state ≺ shouldLock)

    primaryStage.pack()
    primaryStage setVisible true
  }


  private def shouldLock(state : FileWalker.State) : Boolean =
    state != FileWalker.Closing && state != FileWalker.Ready
}


object Application {
  /** Formats a zoom text. */
  private def zoomText(zoom : Option[Double]) : String =
    zoom match {
      case None ⇒ "--"
      case Some(x) ⇒ "%2.2f%%".format(x)
    }
}
