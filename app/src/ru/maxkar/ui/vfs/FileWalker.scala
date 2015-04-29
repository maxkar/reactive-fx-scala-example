package ru.maxkar.ui.vfs

import ru.maxkar.async._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.proc.Activator
import ru.maxkar.reactive.value._

import ru.maxkar.util.vfs._

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


  /** Flag indicating that this walker is in active operation. */
  private var inOp = false

  /** All entities in the current directory. */
  private val allEntitiesV = variable[Set[FileInfo]](baseEntities)

  /** Current directory view model. */
  private val curDirectoryV = variable(baseDirectory)

  /** Items in this file walker, these items are available for the UI. */
  val items = allEntitiesV.behaviour ≺ (ents ⇒ ents.toSeq.sortWith(isBefore))



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
  def select(entity : FileInfo) : Unit =
    if (!inOp && (allEntitiesV.value.contains(entity) || entity == null))
      selectionV.set(entity)



  /** Opens current selection and navigates inside it. */
  def open() : Unit =
    openItem(selection.value)



  /** Opens a random item. */
  def openItem(item : FileInfo) : Unit = {
    if (inOp || item == null || !allEntitiesV.value.contains(item))
      return
    val ename = curDirectoryV.value.dirName
    val res = async {
      item match {
        case FileInfo.ParentDirectory ⇒
          val p = curDirectoryV.value.getParent()
          curDirectoryV.value.close()
          loadContent(p)
        case FileInfo.NestedItem(_, e) ⇒
          if (!e.container)
            null
          else {
            loadContent(e.open())
          }
      }
    }

    res.onComplete(v ⇒ inOp = false)
    res.onSuccess(v ⇒ {
      if (v != null)
        Activator.batch {
          allEntitiesV.set(v._1)
          val nd =
            if (item != FileInfo.ParentDirectory)
              FileInfo.ParentDirectory
            else
              v._1.find(x ⇒ x.name == ename).getOrElse(null)
          selectionV.set(nd)
          curDirectoryV.set(v._2)
        }
    })
  }



  /**
   * Closes this file walker. Should be called when
   * theer are no active operations. */
  def close() : Promise[Unit] = {
    if (inOp)
      return immediate(())
    inOp = true
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

  /** Provides relative type ordering. */
  private def typeOrd(t : FileType) : Int =
    t match {
      case FileType.ParentDirectory ⇒ 1
      case FileType.Directory ⇒ 2
      case _ ⇒ 3
    }



  /** Entity sort order. */
  private def isBefore(e1 : FileInfo, e2 : FileInfo) : Boolean = {
    val typeVal = Integer.compare(typeOrd(e1.fileType), typeOrd(e2.fileType))
    if (typeVal != 0)
      return typeVal < 0
    return e1.name.compareTo(e2.name) < 0
  }



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
