package ru.maxkar.widgets.vfs

import ru.maxkar.async._

import ru.maxkar.fun.syntax._
import ru.maxkar.reactive.wave._
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

  /** Focused (selected) entity. */
  private val selectionV = variable[FileInfo](null)


  /* PUBLIC API. */

  /** Items in this file walker, these items are available for the UI. */
  val items = allEntitiesV.behaviour ≺ (ents ⇒ ents.toSeq.sortWith(isBefore))

  /** Selected entity model. */
  val selection : Behaviour[FileInfo] = selectionV



  /**
   * Selects (focuses) a new entity. Do not perform any
   * navigation actions during this operation but changes active
   * entity.
   */
  def select(entity : FileInfo) : Unit =
    if (!inOp && allEntitiesV.value.contains(entity))
      selectionV.set(entity)



  /** Opens current selection and navigates inside it. */
  def open() : Unit = {
    val item = selection.value
    if (inOp || item == null)
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
        Wave.group(w ⇒ {
          allEntitiesV.wavedSet(v._1, w)
          val nd =
            if (item != FileInfo.ParentDirectory)
              FileInfo.ParentDirectory
            else
              v._1.find(x ⇒ x.name == ename).getOrElse(null)
          selectionV.wavedSet(nd, w)
          curDirectoryV.wavedSet(v._2, w)
        })
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

    res.onComplete(res ⇒ Wave.group(w ⇒ {
      allEntitiesV.wavedSet(Set.empty, w)
      curDirectoryV.wavedSet(null, w)
      selectionV.wavedSet(null, w)
    }))

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
