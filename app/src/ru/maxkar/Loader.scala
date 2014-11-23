package ru.maxkar

import ru.maxkar.async._

import ru.maxkar.fx._

import javafx.application.Application
import javafx.application.Platform
import javafx.scene.layout._

import javafx.stage._
import javafx.scene.text._
import javafx.scene._


import ru.maxkar.util.vfs._

import scala.collection.mutable._


final class Loader extends Application {
  override def start(primaryStage : Stage) : Unit = {
    val shutdownHandlers = new Stack[() ⇒ Promise[Any, Any]]
    val iohandler = new AsyncExecutor(Platform.runLater)
    shutdownHandlers.push(iohandler.shutdown)

    var root = new BorderPane()
    root setCenter new Text("Loading")

    primaryStage setScene new Scene(root, 300, 100)
    primaryStage.show()

    var x = DirectoryBrowser.open(iohandler, new java.io.File("."))

    x.onSuccess(x ⇒ {
      shutdownHandlers.push(x.shutdown)
      new FXApp(
        iohandler, x,
        () ⇒ Loader.shutdown(shutdownHandlers)).start()
      primaryStage.hide()
    })
  }
}



/**
 * Application loader object.
 */
object Loader extends App {
  override def main(args : Array[String]) : Unit =
    Application.launch(classOf[Loader], args: _*)



  /** Shutdowns platform completely. */
  def shutdown(handlers : Stack[() ⇒ Promise[Any, Any]]) : Unit = {
    if (handlers.isEmpty)
      Platform.exit()
    else
      handlers.pop()().onComplete(_ ⇒ shutdown(handlers))
  }
}
