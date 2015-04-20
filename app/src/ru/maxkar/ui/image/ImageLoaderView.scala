package ru.maxkar.ui.image

import java.awt.image.BufferedImage

import javax.swing.JComponent
import javax.swing.JPanel
import javax.swing.JTextArea

import ru.maxkar.ui._
import ru.maxkar.util.Exceptions
import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.value.syntax._
import ru.maxkar.async._


/**
 * Image loader companion.
 */
object ImageLoaderView {
  import java.io.File


  private val FADE_DELAY = 75
  private val FADE_TIME = 500

  /**
   * Convenience method. Creates a new loader and
   * image view.
   */
  def autoMake(
        async : Promising,
        file : Behaviour[File],
        zoom : Behaviour[Zoom])(
        implicit ctx : BindContext) :
      (JComponent, Behaviour[Option[Double]]) = {
    val image = simpleAttractor(file, ImageLoader.loadFile, async) : Behaviour[AttractorState[BufferedImage]]
    val shouldTween = image ≺ (_ == Updating)
    val uiTween = Tween.linear(FADE_DELAY + FADE_TIME, 25, shouldTween)

    val internalState = mkInternalState(image, uiTween)
    val displayImg = internalState ≺ getDisplayImage
    val (imgUI, imgZoom) = ImagePane.render(displayImg, zoom, fadeAmount _ ≻ uiTween)
    val ui = Controls.contentOf(stateUI(imgUI) _ ≻ internalState)
    val effectiveZoom = selectZoom(imgZoom) _ ≽ internalState

    (ui, effectiveZoom)
  }



  /** Selects a zoom behaviour. */
  private def selectZoom(
        default : Behaviour[Option[Double]])(
        state : AttractorState[BufferedImage])
      : Behaviour[Option[Double]] =
    state match {
      case Success(_) ⇒ default
      case _ ⇒ const(None)
    }



  /** Returns UI for the state.
   * @param imgUI image display UI.
   * @param state current UI state.
   * @return UI to use for the state.
   */
  private def stateUI(
        imgUI : JComponent)(
        state : AttractorState[BufferedImage])
      : JComponent =
    state match {
      case Updating | Success(null) ⇒ new JPanel()
      case Success(_) ⇒ imgUI
      case Failure(e) ⇒
        val msg = Exceptions.fullException(e)
        val ta = new JTextArea(msg)
        ta setEditable false
        ta
    }




  /** Calculates a fade amount. */
  private def fadeAmount(x : Int) : Double =
    if (x < FADE_DELAY)
      1
    else
      (FADE_TIME - (x - FADE_DELAY)) / FADE_TIME.toDouble



  /** Creates an internal state for the display image. */
  private def mkInternalState(
        state : Behaviour[AttractorState[BufferedImage]],
        tween : Behaviour[Int])(
        implicit ctx : BindContext)
      : Behaviour[AttractorState[BufferedImage]] = {
    var lastImg : BufferedImage = null
    def update(st : AttractorState[BufferedImage])(tween : Int)
        : AttractorState[BufferedImage] =
      st match {
        case Success(x) ⇒
          lastImg = x
          st
        case Updating ⇒
          if (tween < FADE_DELAY + FADE_TIME) Success(lastImg) else Updating
        case x ⇒ x
      }

    update _ ≻ state ≻ tween
  }



  /** Returns an image to display. */
  private def getDisplayImage(state : AttractorState[BufferedImage]) : BufferedImage =
    state match {
      case Success(x) ⇒ x
      case _ ⇒ null
    }
}
