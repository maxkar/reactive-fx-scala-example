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
    val res = navTo(curDirectoryV.value, item)
    if (res == null)
      return

    stateV.set(Nav)
    res.onComplete(v ⇒ stateV.set(Ready))
    res.onSuccess(v ⇒ Activator.batch {
      allEntitiesV.set(v._1)
      curDirectoryV.set(v._2)
      selectionV.set(v._3)
    })
  }



  /** Navigates to a sibling folder. */
  def siblingNavNext() : Unit = {
    if (stateV.value != Ready || selection.value == null)
      return
    if (!allEntitiesV.value.contains(FileInfo.ParentDirectory))
      return

    val res = navTo(curDirectoryV.value, FileInfo.ParentDirectory)
    if (res == null)
      return

    stateV.set(CrossNav)
    res.onFailure(v ⇒ stateV.set(Ready))
    res.onSuccess(initXNavInDir)
  }



  /** Cross-navigates to a next displayable item (possible in another directory). */
  def crossNavNext() : Unit = {
    if (stateV.value != Ready || selection.value == null)
      return

    getNextXNav(items.value, selection.value) match {
      case null ⇒ return
      case a@FileInfo.NestedItem(FileType.Image, _) ⇒
        selectionV.set(a)
      case cnt ⇒
        val res = navTo(curDirectoryV.value, cnt)
        if (res == null)
          return
        stateV.set(CrossNav)
        res.onFailure(v ⇒ stateV.set(Ready))
        res.onSuccess(initXNavInDir)
    }
  }



  /** Initializes a cross-nav in the new directory. */
  private def initXNavInDir(info : (Set[FileInfo], DirectoryView, FileInfo)) : Unit =
    Activator.batch {
      allEntitiesV.set(info._1)
      curDirectoryV.set(info._2)
      selectionV.set(info._3)

      val fullOrder = sort(info._1)(sorting.value)
      getNextXNav(fullOrder, info._3) match {
        case null ⇒ ()
        case a@FileInfo.NestedItem(FileType.Image, _) ⇒
          selectionV.set(a)
          stateV.set(Ready)
        case cnt ⇒
          val res = navTo(info._2, cnt)
          if (res == null) {
            stateV.set(Ready)
          } else {
            res.onFailure(v ⇒ stateV.set(Ready))
            res.onSuccess(initXNavInDir)
          }
      }
    }



  /**
   * Finds a next cross-nav item from list of candidates.
   */
  private def getNextXNav(items : Seq[FileInfo], cur : FileInfo) : FileInfo = {
    val rest = items.dropWhile(item ⇒ item != cur).tail
    rest.find(isXNavigable).getOrElse(
      if (items.contains(FileInfo.ParentDirectory)) FileInfo.ParentDirectory else null)
  }



  /**
   * Navigates into the subriderctory and returs list of items and current
   * item in that directory.
   */
  private def navTo(curDir : DirectoryView, item : FileInfo) : Promise[(Set[FileInfo], DirectoryView, FileInfo)] = {
    item match {
      case FileInfo.ParentDirectory ⇒
        async {
          val ename = curDir.dirName
          val p = curDir.getParent()
          curDir.close()
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
  case object Nav extends State
  /** Cross-navigation. */
  case object CrossNav extends State
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



  /** Checks if file info is eligible for cross-navigation. */
  private def isXNavigable(item : FileInfo) : Boolean =
    item match {
      case FileInfo.NestedItem(FileType.Image, _) ⇒ true
      case FileInfo.NestedItem(FileType.Container, _) ⇒ true
      case FileInfo.NestedItem(FileType.Directory, _) ⇒ true
      case _ ⇒ false
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
