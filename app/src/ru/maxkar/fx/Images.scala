package ru.maxkar.fx

import javafx.geometry._
import javafx.scene.Node
import javafx.scene.image._
import javafx.scene.control._
import javafx.scene.layout._


import ru.maxkar.widgets.zoom.Zoom
import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import Bridge._

/** Image utilities. */
object Images {
  /** Creates a simple image view for the image behaviour. */
  def simpleView(
        state : Behaviour[Image])(
        implicit lifespan : Lifespan)
      : Node = {
    val res = new ImageView
    res.setImage _ :> state
    res
  }


  /**
   * Creates a "smart" image view with pannable image view
   * and automatic image centering.
   */
 def imageView(
        state : Behaviour[Image],
        zoom : Behaviour[Zoom])(
        implicit lifespan : Lifespan)
      : (Region, Behaviour[Double]) = {
    val img = new ImageView
    img setPreserveRatio true
    img setSmooth true
    img.setImage _ :> state

    val content = Layouts.floatCenter(img)

    val sp = new ScrollPane(content)
    sp setPannable true
    sp setVbarPolicy ScrollPane.ScrollBarPolicy.NEVER
    sp setHbarPolicy ScrollPane.ScrollBarPolicy.NEVER
    sp setFitToWidth true
    sp setFitToHeight true

    val effectiveZoom =
      applyZoom(img) _ :> zoom :> state :> sp.widthProperty :> sp.heightProperty

    (sp, effectiveZoom)
  }


  /** Applies zoom to an image. */
  private def applyZoom(
        ui : ImageView)(zoom : Zoom)(
        image : Image)(
        width : Number)(height : Number) : Double = {
    if (image == null)
      return 1

    val zoomLevel = zoom.scaleFor(
        image.getWidth, image.getHeight,
        width.doubleValue - 2, height.doubleValue - 2)

    ui.setFitWidth(zoomLevel * image.getWidth)
    ui.setFitHeight(zoomLevel * image.getHeight)
    return zoomLevel
  }
}

