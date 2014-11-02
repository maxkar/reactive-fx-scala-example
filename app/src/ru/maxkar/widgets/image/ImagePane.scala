package ru.maxkar.widgets.image

import javafx.geometry._
import javafx.scene.image._
import javafx.scene.control._
import javafx.scene.layout._

import ru.maxkar.fx.Layouts
import ru.maxkar.fx.Bridge._

import ru.maxkar.widgets.zoom._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

/**
 * Pane used to display one (and only one) image.
 * @param image image to display.
 * @param zoom zoom model.
 * @param lifespan image/binding lifespan.
 */
final class ImagePane(
      image : Image,
      zoom : Behaviour[Zoom])(
      implicit ctx : BindContext) {

  /** Image display. */
  private val view = new ImageView(image)
  view setPreserveRatio true
  view setSmooth true


  /** Inner UI display. */
  private val innerContent = Layouts.floatCenter(view)


  /** Scroll pane for the image pane. */
  private val sp = new ScrollPane(innerContent)
  sp setPannable true
  sp setVbarPolicy ScrollPane.ScrollBarPolicy.NEVER
  sp setHbarPolicy ScrollPane.ScrollBarPolicy.NEVER
  sp setFitToWidth true
  sp setFitToHeight true


  /** User-accessible UI content. */
  val ui : Region = sp


  /** Effective zoom value. */
  val effectiveZoom =
    calcEffectiveZoom _ :> zoom :> sp.widthProperty :> sp.heightProperty
  effectiveZoom :< applyZoom


  /* METHODS. */


  /** Calculates an effective zoom level using given
   * zoom settings. */
  private def calcEffectiveZoom(
        zoom : Zoom)(
        width : Number)(
        height : Number)
      : Double =
    zoom.scaleFor(image.getWidth, image.getHeight,
      width.doubleValue - 2, height.doubleValue - 2)


  /** Applies zoom to image. */
  private def applyZoom(level : Double) : Unit = {
    view setFitWidth level * image.getWidth
    view setFitHeight level * image.getHeight
  }
}
