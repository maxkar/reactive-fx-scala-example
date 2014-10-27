package ru.maxkar

import java.io.File

import javafx.application.Application
import javafx.application.Platform
import javafx.geometry._
import javafx.stage._
import javafx.scene._
import javafx.scene.control._
import javafx.scene.layout._


import ru.maxkar.fx._
import ru.maxkar.fx.Bridge._

import ru.maxkar.lib.reactive.value.Lifespan
import ru.maxkar.lib.reactive.value.Behaviour._

import scala.collection.JavaConversions._



class FXApp extends Application {
  /** Application-wide lifespan. */
  private implicit val lifespan = Lifespan.forever

  /** IO executor. */
  private val iohandler = new AsyncExecutor(Platform.runLater)

  override def start(primaryStage : Stage) : Unit = {
    val root = new BorderPane()
    val file = variable[File](null)
    val fc = new FileChooser()

    val imageui = new ru.maxkar.widgets.BasicImage(iohandler, file)


    val gb = new GridPane()
    gb.add(imageui.node, 0, 0)
    GridPane.setHalignment(imageui.node, HPos.CENTER)
    GridPane.setValignment(imageui.node, VPos.CENTER)
    GridPane.setHgrow(imageui.node, Priority.ALWAYS)
    GridPane.setVgrow(imageui.node, Priority.ALWAYS)
    gb setGridLinesVisible(true)
    GridPane.setFillWidth(imageui.node, true)

    val sp = new ScrollPane(gb)
    sp setPannable true
    sp setVbarPolicy ScrollPane.ScrollBarPolicy.NEVER
    sp setHbarPolicy ScrollPane.ScrollBarPolicy.NEVER
    sp setFitToWidth true
    sp setFitToHeight true

    root setCenter sp

    val opText = iohandler.operationCount :< (x ⇒ "IO ops: " + x)
    root setBottom (Texts.simpleText(opText))
    root setTop Buttons.simplestButton("Open", {
      val f = fc.showOpenDialog(primaryStage)
      if (f != null)
        file.set(f)
    })

    primaryStage setOnCloseRequest shutdownAll

    val scene = new Scene(root, 500, 500)
    primaryStage setTitle "JavaFX Scene Graph Demo"
    primaryStage setScene scene
    primaryStage.show()
  }

  private def shutdownAll() : Unit = {
    iohandler.shutdown.onComplete(_ ⇒ Platform.exit())
  }
}


object FXApp extends App{
  override def main(args : Array[String]) : Unit =
    Application.launch(classOf[FXApp], args: _*)
}
