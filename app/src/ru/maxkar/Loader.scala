package ru.maxkar

import java.io.File

import ru.maxkar.async._

import ru.maxkar.fx._

import javafx.application.Application
import javafx.application.Platform
import javafx.scene.layout._

import javafx.stage._
import javafx.scene.text._
import javafx.scene._
import javafx.scene.image.Image


import ru.maxkar.util.vfs._
import ru.maxkar.widgets.vfs._
import ru.maxkar.widgets.image.ImageLoader

import scala.collection.mutable.Stack


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
    var y = iohandler(Loader.prepareFSRenderer())

    y.onSuccess(y ⇒ {
      x.onSuccess(x ⇒ {
        shutdownHandlers.push(x.shutdown)
        new FXApp(
          iohandler, x, y,
          () ⇒ Loader.shutdown(shutdownHandlers)).start()
        primaryStage.hide()
      })
    })
  }
}



/**
 * Application loader object.
 */
object Loader extends App {
  override def main(args : Array[String]) : Unit =
    Application.launch(classOf[Loader], args: _*)



  def prepareFSRenderer() : BrowserViewRenderer = {
    val base = "/usr/share/icons/gnome/24x24/"
    BrowserViewRenderer.make(
      dirIcon = mkImage(base + "places/folder.png"),
      archiveIcon = mkImage(base + "mimetypes/folder_tar.png"),
      imageIcon = mkImage(base + "mimetypes/image.png"),
      imageUnknown = mkImage(base + "mimetypes/unknown.png"))
  }



  private def mkImage(path : String) : Image =
    ImageLoader.loadFile(new File(path))



  /** Shutdowns platform completely. */
  def shutdown(handlers : Stack[() ⇒ Promise[Any, Any]]) : Unit = {
    if (handlers.isEmpty)
      Platform.exit()
    else
      handlers.pop()().onComplete(_ ⇒ shutdown(handlers))
  }
}
