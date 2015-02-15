package ru.maxkar

import java.io.File

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.{PosixFilePermissions ⇒ Perms}

import java.awt.image.BufferedImage
import java.awt.BorderLayout

import javax.swing.SwingUtilities
import javax.swing.JLabel
import javax.swing.JFrame

import ru.maxkar.async._

import ru.maxkar.fun._
import ru.maxkar.fun.syntax._

import ru.maxkar.util._
import ru.maxkar.util.Runnables._
import ru.maxkar.util.vfs._
import ru.maxkar.ui.vfs._
import ru.maxkar.ui.image.ImageLoader

import scala.collection.mutable.Stack


final class Loader {
  def start() : Unit = {
    val loadingWindow = new JFrame("Loading")

    val shutdownHandlers = new Stack[() ⇒ Promise[Any]]
    val iohandler = new AsyncExecutor(SwingUtilities.invokeLater)
    shutdownHandlers.push(iohandler.shutdown)

    loadingWindow.getContentPane().add(new JLabel("Loading"))
    loadingWindow.pack()
    loadingWindow setVisible true

    val fs = iohandler(Loader.createMountPoint())
    fs ≺ (mp ⇒
      shutdownHandlers.push(
        () ⇒ iohandler(Files.delete(mp))))
    val style = Loader.prepareFSRenderer()

    val walker = fs ≼ (mp ⇒
      FileWalker.open(
        iohandler,
        FuseMounter.inPath(mp),
        new java.io.File(".")))
    walker ≺ (w ⇒ shutdownHandlers.push(() ⇒ w.close()))

    def launch(w : FileWalker)(style : (BufferedImage, BufferedImage, BufferedImage, BufferedImage)) : Unit = {
      new Application(
        iohandler, w, style,
        () ⇒ Loader.shutdown(shutdownHandlers)).start()
      loadingWindow.dispose()
    }
    launch _ ≻ walker ≻ style
  }
}



/**
 * Application loader object.
 */
object Loader extends App {
  override def main(args : Array[String]) : Unit =
    SwingUtilities.invokeLater(() ⇒ new Loader().start())



  def prepareFSRenderer() : (BufferedImage, BufferedImage, BufferedImage, BufferedImage) = {
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



  private def mkImage(path : String) : BufferedImage =
    ImageLoader.loadFile(new File(path))



  /** Shutdowns platform completely. */
  def shutdown(handlers : Stack[() ⇒ Promise[Any]]) : Unit = {
    try {
      if (!handlers.isEmpty)
        handlers.pop()().onComplete(_ ⇒ shutdown(handlers))
    } catch {
      case e : Throwable ⇒ e.printStackTrace()
    }
  }
}
