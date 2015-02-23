package ru.maxkar.ui.image

import javax.swing.JComponent
import javax.swing.JPanel
import javax.swing.JTextArea

import ru.maxkar.ui._
import ru.maxkar.util.Exceptions
import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._
import ru.maxkar.reactive.value.syntax._


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
  val ui : JComponent =
    Controls contentOf (getUI _ ≻ renderState)



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
        UIImage(ImagePane.render(x, zoom))
      case ImageLoader.Loading ⇒
        UIOther(new JPanel())
        //UIOther(new ProgressIndicator())
      case ImageLoader.LoadError(e) ⇒
        val msg =
          if (e == null)
            "No Image loaded, bad format?"
          else
            Exceptions.fullException(e)
        val ta = new JTextArea(msg)
        ta setEditable false
        UIOther(ta)
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
  private case class UIOther(ui : JComponent) extends UIState



  /**
   * Calculates an UI for the state.
   */
  private def getUI(state : UIState) : JComponent =
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