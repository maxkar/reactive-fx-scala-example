package ru.maxkar.ui.vfs.sort

import ru.maxkar.ui.Controls

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.value._

import ru.maxkar.ui.syntax._
import ru.maxkar.ui.vfs._


/** File list sorting UI. */
private[vfs] final class SortUI(walker : FileWalker)(implicit ctx : BindContext) {
  /** "Sorting is displayed" flag. */
  private val displayed = variable(false)


  /** Sorting combo. */
  private val combo = Controls.combo(SortSpec.allItems, walker.sorting, walker.sortBy)

  /** Public component UI. */
  val ui = {
    val res = Controls.contentOf(displayed.behaviour ≺ (x ⇒ if (x) combo else null))
    res.actions ++= Seq("toggle" :-> toggle())
    res.keysWhenFocusedAncestor ++= Seq("ctrl S" → "toggle")
    res
  }


  /** Toggles a display. */
  def toggle() : Unit = {
    val old = displayed.latestValue
    displayed set !old
    if (!old)
      combo.requestFocusInWindow()
  }
}



/** Sorting UI. */
private[vfs] object SortUI {
  /** Creates a new sorting UI. */
  def make(walker : FileWalker)(implicit ctx : BindContext) : SortUI =
    new SortUI(walker)
}
