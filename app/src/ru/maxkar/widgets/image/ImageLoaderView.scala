package ru.maxkar.widgets.image

import javafx.scene._
import javafx.scene.layout._
import javafx.scene.text.Text
import javafx.scene.control.ProgressIndicator

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.value.syntax._

import ru.maxkar.widgets.zoom.Zoom

import ru.maxkar.fx._

/**
 * View for the image loader.
 */
final class ImageLoaderView(
      image : Behaviour[ImageLoader.State],
      zoom : Behaviour[Zoom])(
      implicit ctx : BindContext){
  import ImageLoaderView._


  /** Rendered image. */
  private val renderState = image ≺~ render



  /** Image loader view. */
  val ui : Node =
    Nodes regionOf (getUI _ ≻ renderState)



  /** Effective zoom state. IT is set only
   * when image is displayed. Set to None
   * in all other cases.
   */
  val effectiveZoom : Behaviour[Option[Double]] =
    effectiveZoomOf _ ~≽ renderState



  /**
   * Renders a loader state.
   */
  private def render(
        state : ImageLoader.State,
        ctx : BindContext) : UIState = {
    implicit val c = ctx

    state match {
      case ImageLoader.Ready(null) ⇒
        UIOther(null)
      case ImageLoader.Ready(x) ⇒
        UIImage(new ImagePane(x, zoom))
      case ImageLoader.Loading ⇒
        UIOther(new ProgressIndicator())
      case ImageLoader.LoadError(e) ⇒
        val msg =
          if (e == null)
            "No Image loaded, bad format?"
          else
            Exceptions.fullException(e)
        UIOther(new Text(msg))
    }
  }
}



/**
 * Image loader companion.
 */
object ImageLoaderView {
  import java.io.File
  import ru.maxkar.async.Promising

  private abstract sealed class UIState
  private case class UIImage(img : ImagePane) extends UIState
  private case class UIOther(ui : Node) extends UIState



  /**
   * Calculates an UI for the state.
   */
  private def getUI(state : UIState) : Node =
    state match {
      case UIImage(x) ⇒ x.ui
      case UIOther(x) ⇒ x
    }



  /**
   * Calculates an effective zoom for the UI.
   */
  private def effectiveZoomOf(
        state : UIState,
        ctx : BindContext) :
      Behaviour[Option[Double]] = {
    implicit val c = ctx
    state match {
      case UIImage(x) ⇒ x.effectiveZoom ≺ (z ⇒ Some(z))
      case _ ⇒ const(None)
    }
  }



  /**
   * Convenience method. Creates a new loader and
   * image view.
   */
  def autoMake(
        async : Promising,
        file : Behaviour[File],
        zoom : Behaviour[Zoom])(
        implicit ctx : BindContext) :
      ImageLoaderView =
    new ImageLoaderView(
      new ImageLoader(async, file).state,
      zoom)
}
