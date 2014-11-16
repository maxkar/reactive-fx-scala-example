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

import ru.maxkar.lib.reactive.value.Behaviour
import ru.maxkar.lib.reactive.value.Behaviour._

import scala.collection.JavaConversions._



class FXApp extends Application {
  import FXApp._


  /** Application-wide lifespan. */
  private implicit val bindContext = Behaviour.defaultBindContext

  /** IO executor. */
  private val iohandler = new AsyncExecutor(Platform.runLater)


  /** Zoom level presets. */
  private val zoomLevels = Seq(0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0)



  override def start(primaryStage : Stage) : Unit = {
    val root = new BorderPane()
    val file = variable[File](null)
    val fc = new FileChooser()

    val zoom = variable[Zoom](Zoom.Fixed(1.0))

    val imageui = ImageLoaderView.autoMake(iohandler, file, zoom)
    root setCenter imageui.ui

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
    root setTop Buttons.simplestButton("Open", {
      val pp = file.value
      if (pp != null && pp.getParentFile != null)
        fc.setInitialDirectory(pp.getParentFile)
      val f = fc.showOpenDialog(primaryStage)
      if (f != null)
        file.set(f)
    })


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

    primaryStage setOnCloseRequest shutdownAll

    val scene = new Scene(root, 500, 500)
    primaryStage setTitle "JavaFX Scene Graph Demo"
    primaryStage setScene scene
    primaryStage.show()
  }



  /** Shutdowns the application. */
  private def shutdownAll() : Unit = {
    iohandler.shutdown.onComplete(_ ⇒ Platform.exit())
  }
}


object FXApp extends App{
  override def main(args : Array[String]) : Unit =
    Application.launch(classOf[FXApp], args: _*)



  /** Formats a zoom text. */
  private def zoomText(zoom : Option[Double]) : String =
    zoom match {
      case None ⇒ "--"
      case Some(x) ⇒ "%2.2f%%".format(x)
    }
}
