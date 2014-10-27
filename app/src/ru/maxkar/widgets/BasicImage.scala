package ru.maxkar.widgets

import java.io.File

import javafx.scene._
import javafx.scene.image._
import javafx.scene.text._
import javafx.scene.control.ProgressIndicator

import ru.maxkar.async._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import ru.maxkar.fx._

/**
 * Basic image display capability.
 * @param async asynchronous executor.
 * @param file current file to display.
 */
final class BasicImage(
      async : Promising[Throwable],
      file : Behaviour[File])(
      implicit lifespan : Lifespan) {
  import BasicImage._

  /* Model. */

  /** Current image state. */
  private val state = variable[State](Ready(null))



  /**
   * Loads a file and updates a state when everything is ready.
   */
  private def load(file : File) : Unit = {
    state.value match {
      case Loading | Reload(_) ⇒ state.set(Reload(file))
      case _ ⇒ doLoad(file)
    }
  }



  /**
   * Load implementation.
   */
  private def doLoad(file : File) : Unit = {
    if (file == null) {
      state.set(Ready(null))
      return
    }

    state.set(Loading)
    async(loadFile(file)).onComplete(updateAfterLoad)
  }



  /** Updates state after data are loaded. */
  private def updateAfterLoad(res : PromiseResult[Throwable, Image]) : Unit = {
    state.value match {
      case Reload(f) ⇒ doLoad(f)
      case _ ⇒ res match {
        case Success(img) ⇒ state set Ready(img)
        case Failure(x) ⇒ state set Failed(x)
      }
    }
  }


  /** Internal node implementation. */
  val node = Nodes.contentOf(
    chooseDisplay _ :> state :>
      Images.simpleView(imageOf _ :> state) :>
      new ProgressIndicator(-1) :>
      Texts.simpleText(exnOf _ :> state))

  load _ :> file
}

/**
 * Image companion.
 */
private object BasicImage {
  /** Image states. */
  private sealed abstract class State
  private case class Ready(img : Image) extends State
  private case object Loading extends State
  private case class Reload(target : File) extends State
  private case class Failed(error : Throwable) extends State

  /** Returns image for the state. */
  def imageOf(state : State) : Image = state match {
    case Ready(x) ⇒ x
    case _ ⇒ null
  }


  /** Returns text of the state. */
  def exnOf(state : State) : String = state match {
    case Ready(null) ⇒ "No image loaded"
    case Failed(e) ⇒ Exceptions.fullException(e)
    case _ ⇒ null
  }


  /** Loads a file and converts it into an image. */
  def loadFile(file : File) : Image = {
    val img = javax.imageio.ImageIO.read(file)
    val res = javafx.embed.swing.SwingFXUtils.toFXImage(img, null)
    img.flush()
    res
  }


  /** Chooses an UI content to display. */
  def chooseDisplay(
        state : State)(
        forImage : Node)(
        forProgress : Node)(
        forError : Node) : Node = {
    state match {
      case Ready(null) ⇒ forError
      case Ready(x) ⇒ forImage
      case Loading | Reload(_) ⇒ forProgress
      case Failed(x) ⇒ forError
    }
  }
}

