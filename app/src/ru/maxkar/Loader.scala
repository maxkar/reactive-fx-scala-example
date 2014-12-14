package ru.maxkar

import java.io.File

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.{PosixFilePermissions ⇒ Perms}

import ru.maxkar.async._
import ru.maxkar.async.Promise._

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

    val root = new BorderPane()
    root setCenter new Text("Loading")

    primaryStage setScene new Scene(root, 300, 100)
    primaryStage.show()

    val fs = iohandler(Loader.createMountPoint())
    fs.onSuccess(mp ⇒
      shutdownHandlers.push(() ⇒ iohandler(Files.delete(mp))))
    val style = Loader.prepareFSRenderer()

    def openWalker(mountPoint : Path) : Promise[Throwable, FileWalker] =
      FileWalker.open(
        iohandler,
        FuseMounter.inPath(mountPoint),
        new java.io.File("."))
    val walker = openWalker _ :>> fs
    walker.onSuccess(w ⇒ shutdownHandlers.push(() ⇒ w.close()))

    def launch(w : FileWalker)(style : (Image, Image, Image, Image)) : Unit = {
      new FXApp(
        iohandler, w, style,
        () ⇒ Loader.shutdown(shutdownHandlers)).start()
      primaryStage.hide()
    }
    launch _ :> walker :> style
  }
}



/**
 * Application loader object.
 */
object Loader extends App {
  override def main(args : Array[String]) : Unit =
    Application.launch(classOf[Loader], args: _*)



  def prepareFSRenderer() : (Image, Image, Image, Image) = {
    val base = "/usr/share/icons/gnome/24x24/"
    (mkImage(base + "places/folder.png"),
     mkImage(base + "mimetypes/folder_tar.png"),
     mkImage(base + "mimetypes/image.png"),
     mkImage(base + "mimetypes/unknown.png"))
  }



  private def createMountPoint() : Path = {
    val base = Paths.get("/var/tmp/portage")
    Files.createTempDirectory(base, null,
      Perms.asFileAttribute(Perms.fromString("rwx------")))
  }



  private def mkImage(path : String) : Image =
    ImageLoader.loadFile(new File(path))



  /** Shutdowns platform completely. */
  def shutdown(handlers : Stack[() ⇒ Promise[Any, Any]]) : Unit = {
    try {
    if (handlers.isEmpty)
      Platform.exit()
    else
      handlers.pop()().onComplete(_ ⇒ shutdown(handlers))
    } catch {
      case e : Throwable ⇒ e.printStackTrace()
    }
  }
}
