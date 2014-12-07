package ru.maxkar

import java.io.File

import javafx.application.Application
import javafx.application.Platform
import javafx.geometry._
import javafx.stage._
import javafx.scene._
import javafx.scene.control._
import javafx.scene.layout._
import javafx.scene.input._


import ru.maxkar.fx._
import ru.maxkar.fx.Bridge._


import ru.maxkar.widgets.image.ImageLoaderView

import ru.maxkar.widgets.zoom.Zoom
import ru.maxkar.widgets.vfs._

import ru.maxkar.lib.reactive.value.Behaviour
import ru.maxkar.lib.reactive.value.Behaviour._

import scala.collection.JavaConversions._

import ru.maxkar.util.vfs._
import ru.maxkar.widgets.vfs._


class FXApp(
      iohandler : AsyncExecutor,
      bro : DirectoryBrowser,
      fsRenderer : BrowserViewRenderer,
      shutdownHandler : () ⇒ Unit) {
  import FXApp._


  /** Application-wide lifespan. */
  private implicit val bindContext = Behaviour.defaultBindContext


  /** Zoom level presets. */
  private val zoomLevels = Seq(0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0)



  def start() : Unit = {
    var primaryStage = new Stage()
    val root = new BorderPane()
    val fc = new FileChooser()

    val cnt = new SplitPane()

    val tmp = variable[widgets.vfs.BrowserViewRenderer.Item](null)

    val curFile = variable[DirectoryEntry](null)
    val fsRender = bro.state :/< ((s, c) ⇒ fsRenderer.render(s, tmp, tmp.set)(c))
    val fsview = Nodes.regionOf(fsRender)
    fsRender :< (_.requestFocus())


    fsview.addEventFilter(KeyEvent.KEY_PRESSED,
      (e : KeyEvent) ⇒ {
        val v = tmp.value
        if (e.getCode == KeyCode.ENTER && v != null) {
          e.consume()
          v match {
            case BrowserViewRenderer.ParentEntry(x) ⇒ x()
            case BrowserViewRenderer.NestedEntry(x) ⇒ bro.enter(x)
          }
        }
      })

    val file = tmp :< (item ⇒
      if (item == null)
        null
      else
        item match {
          case BrowserViewRenderer.ParentEntry(x) ⇒ null
          case BrowserViewRenderer.NestedEntry(x) ⇒
            if (x.filestream)
              x.backingFile()
            else
              null
        }
      )

    val zoom = variable[Zoom](Zoom.Fixed(1.0))

    val imageui = ImageLoaderView.autoMake(iohandler, file, zoom)

    val opText = iohandler.operationCount :< (x ⇒ "IO ops: " + x)

    val zoomBox = new ComboBox[Zoom]()
    zoomBox.getItems().addAll(
      (Zoom.SmartFit +: Zoom.Fit +: zoomLevels.map(x ⇒ Zoom.Fixed(x))))
    bind(zoomBox.valueProperty, zoom, zoom.set)

    val bottom = new HBox()
    bottom setSpacing 10
    bottom setPadding new Insets(0, 10, 0, 10)
    bottom setAlignment Pos.CENTER_LEFT
    bottom.getChildren().addAll(
      Texts.simpleText(opText),
      zoomBox,
      Texts.simpleText(zoomText _ :> imageui.effectiveZoom))


    root setBottom bottom

    cnt.getItems.addAll(fsview, imageui.ui)
    root setCenter cnt


    root.addEventFilter(KeyEvent.KEY_PRESSED,
      (e : KeyEvent) ⇒ {
        val newZoom = Zoom.zoomForShortcut(
          zoomLevels, imageui.effectiveZoom.value, zoom.value,
          e.getCode)
        newZoom match {
          case Some(x) ⇒
            zoom set x
            e.consume()
          case _ ⇒ ()
        }
      })

    primaryStage.setOnCloseRequest((e : WindowEvent) ⇒ {
      e.consume()
      shutdownHandler()
    })

    val scene = new Scene(root, 500, 500)
    primaryStage setTitle "JavaFX Scene Graph Demo"
    primaryStage setScene scene
    primaryStage.show()
  }
}


object FXApp {
  /** Formats a zoom text. */
  private def zoomText(zoom : Option[Double]) : String =
    zoom match {
      case None ⇒ "--"
      case Some(x) ⇒ "%2.2f%%".format(x)
    }
}
