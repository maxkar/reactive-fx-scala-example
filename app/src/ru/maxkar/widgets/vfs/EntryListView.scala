package ru.maxkar.widgets.vfs

import ru.maxkar.fx.Bridge._

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import javafx.scene.layout.Region
import javafx.scene.control.ListView

import ru.maxkar.util.vfs._

/**
 * Entry list view widgets and utilities.
 */
final object EntryListView {
  def render(
        list : Seq[DirectoryEntry],
        current : Behaviour[DirectoryEntry],
        selector : DirectoryEntry ⇒ Unit)(
        implicit ctx : BindContext)
      : Region = {
    val view = new ListView(list.asFXList)
    bindRO(
      view.getSelectionModel().selectedItemProperty(),
      (x : DirectoryEntry) ⇒ view.getSelectionModel().select(x),
      current, selector)
    view
  }
}
