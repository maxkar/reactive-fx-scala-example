package ru.maxkar.ui.vfs

import ru.maxkar.async._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.proc.Activator
import ru.maxkar.reactive.value._

import ru.maxkar.util.vfs._

import sort._

/**
 * Filesystem walker model. Walks through file system
 * in the "UI-friendly" manner.
 * @param async asynchronous executor.
 */
final class FileWalker private(
      async : Promising,
      baseEntities : Set[FileInfo],
      baseDirectory : DirectoryView) {

  import FileWalker._
  private implicit val ctx = permanentBind

  /** Current state of the walker. */
  private var stateV = variable[State](Ready)

  /** All entities in the current directory. */
  private val allEntitiesV = variable[Set[FileInfo]](baseEntities)

  /** Current directory view model. */
  private val curDirectoryV = variable(baseDirectory)

  /** Sorting order variable. */
  private val sortingV = variable[SortSpec](SortNumCS)


  /** Current (read-only) state of the file walker. */
  var state = stateV.behaviour

  /** Selecterd sorting mode. */
  val sorting = sortingV.behaviour

  /** Items in this file walker, these items are available for the UI. */
  val items = sort _ ≻ allEntitiesV.behaviour ≻ sorting


  /** Focused (selected) entity. */
  private val selectionV = variable[FileInfo](
    if (items.value.size == 0) null else items.value()(0))


  /** Selected entity model. */
  val selection : Behaviour[FileInfo] = selectionV



  /**
   * Selects (focuses) a new entity. Do not perform any
   * navigation actions during this operation but changes active
   * entity.
   */
  def select(entity : FileInfo) : Unit = {
    if (stateV.value != Ready)
      return
    if (allEntitiesV.value.contains(entity) || entity == null)
      selectionV.set(entity)
  }



  /**
   * Sets a new sorting order.
   */
  def sortBy(sort : SortSpec) : Unit =
    sortingV set sort



  /** Opens current selection and navigates inside it. */
  def open() : Unit =
    openItem(selection.value)



  /** Opens a random item. */
  def openItem(item : FileInfo) : Unit = {
    if (stateV.value != Ready || item == null || !allEntitiesV.value.contains(item))
      return
    val res = navTo(item)
    if (res == null)
      return

    stateV.set(SyncNav)
    res.onComplete(v ⇒ stateV.set(Ready))
    res.onSuccess(v ⇒ Activator.batch {
      allEntitiesV.set(v._1)
      curDirectoryV.set(v._2)
      selectionV.set(v._3)
    })
  }


  /**
   * Navigates into the subriderctory and returs list of items and current
   * item in that directory.
   */
  private def navTo(item : FileInfo) : Promise[(Set[FileInfo], DirectoryView, FileInfo)] = {
    item match {
      case FileInfo.ParentDirectory ⇒
        async {
          val ename = curDirectoryV.value.dirName
          val p = curDirectoryV.value.getParent()
          curDirectoryV.value.close()
          val (files, view) = loadContent(p)
          (files, view, files.find(x ⇒ x.name == ename).getOrElse(null))
        }
      case FileInfo.NestedItem(_, e) if !e.container ⇒ null
      case FileInfo.NestedItem(_, e) ⇒
        async {
          val (files, view) = loadContent(e.open())
          (files, view, FileInfo.ParentDirectory)
        }
    }
  }



  /**
   * Closes this file walker. Should be called when
   * theer are no active operations. */
  def close() : Promise[Unit] = {
    if (stateV.value == Ready)
      return immediate(())
    stateV.set(Closing)
    val res = async {
      var p = curDirectoryV.value
      do {
        val next = p.getParent()
        p.close()
        p = next
      } while (p != null)
    }

    res.onComplete(res ⇒ Activator.batch {
      allEntitiesV.set(Set.empty)
      curDirectoryV.set(null)
      selectionV.set(null)
    })

    res
  }
}



/**
 * Filesystem walker companion object.
 */
object FileWalker {

  /** File walker state. */
  abstract sealed class State
  /** File walker is ready and could accept next action. */
  case object Ready extends State
  /** Synchronous navigation is active. */
  case object SyncNav extends State
  /** Walker is closing. */
  case object Closing extends State


  /** Sorts items according to a sort spec. */
  private def sort(items : Set[FileInfo])(ord : SortSpec) : Seq[FileInfo] =
    items.toSeq.sortWith((a, b) ⇒ ord.compare(a, b) < 0)



  /** Loads directory content to use for the new file walker. */
  private def loadContent(dir : DirectoryView) : (Set[FileInfo], DirectoryView) = {
    var filel = dir.listEntries().map(FileInfo.forEntity)
    val allEntities =
      if (dir.getParent() != null)
        FileInfo.ParentDirectory +: filel
      else
        filel
    (allEntities.toSet, dir)
  }



  /**
   * Opens a new file walker with the specified initial (physical)
   * directory.
   * @param async async operation executor.
   * @param mounter file mounter utility.
   * @param base base directory.
   */
  def open(
        async : Promising,
        mounter : FuseMounter,
        base : java.io.File)
      : Promise[FileWalker] =
    async {
      val initialDir = DirectoryView.forFile(base, mounter)
      val (files, d) = loadContent(initialDir)
      new FileWalker(async, files, d)
    }
}
