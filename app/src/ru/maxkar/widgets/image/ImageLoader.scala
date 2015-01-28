package ru.maxkar.widgets.image

import java.io.File
import java.awt.image.BufferedImage


import ru.maxkar.async._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._


/**
 * This class is responsible for loading images and keeping
 * reference to a latest loaded image.
 * @param async executor for asynchronous operations (loadnig)
 * @param file current file to load and display.
 */
final class ImageLoader(
      async : Promising,
      file : Behaviour[File])(
      implicit ctx : BindContext){
  import ImageLoader._

  /** Variable for current state. */
  private val stateV = variable[State](Ready(null))


  /** Next file to load. */
  private var nextFile : File = null


  /** Current loader state. */
  val state : Behaviour[State] = stateV


  load _ ≻ file


  /** Implementations. */

  /** Loads a specified file. */
  private def load(file : File) : Unit = {
    if (state.value == Loading) {
      nextFile = file
      return
    }

    doLoad(file)
  }


  /** Loading implementation. */
  private def doLoad(file : File) : Unit = {
    nextFile = null

    if (file == null) {
      stateV set Ready(null)
      return
    }

    stateV set Loading
    async(loadFile(file)) onComplete applyNewImage
  }



  /** Applies a new image content. */
  private def applyNewImage(res : PromiseResult[BufferedImage]) : Unit = {
    if (nextFile != null)
      doLoad(nextFile)
    else
      res match {
        case Success(x) ⇒ stateV set Ready(x)
        case Failure(x) ⇒ stateV set LoadError(x)
      }
  }
}



/**
 * Image loader companion.
 */
object ImageLoader {
  /** State of the image loader. */
  abstract sealed class State
  /** Image is ready and can be displayed. */
  final case class Ready(image : BufferedImage) extends State
  /** Image is loading. */
  final case object Loading extends State
  /** Image loading failed. */
  final case class LoadError(err : Throwable) extends State



  /** File load implementation. */
  def loadFile(file : File) : BufferedImage =
     javax.imageio.ImageIO.read(file)
}
