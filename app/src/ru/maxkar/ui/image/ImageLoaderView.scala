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
    val displayImg = image ≺ getDisplayImage
    val (imgUI, imgZoom) = ImagePane.render(displayImg, zoom)

    val ui = Controls contentOf (image ≺ (img ⇒
      img match {
        case Updating  | Success(null) ⇒ new JPanel()
        case Success(x) ⇒ imgUI
        case Failure(e) ⇒
          val msg = Exceptions.fullException(e)
          val ta = new JTextArea(msg)
          ta setEditable false
          ta
      }))

    val effectiveZoom : Behaviour[Option[Double]] = image ≼ (img ⇒
      img match {
        case Success(x) ⇒ imgZoom
        case _ ⇒ const(None)
      })

    (ui, effectiveZoom)
  }


  /** Returns an image to display. */
  private def getDisplayImage(state : AttractorState[BufferedImage]) : BufferedImage =
    state match {
      case Success(x) ⇒ x
      case _ ⇒ null
    }

}
