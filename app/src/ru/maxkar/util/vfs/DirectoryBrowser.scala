package ru.maxkar.util.vfs

import java.io.File

import ru.maxkar.lib.reactive.value._
import ru.maxkar.lib.reactive.value.Behaviour._

import ru.maxkar.async._

/**
 * Filesystem and directory browser.
 * Provides a standard "navigator-like" api to walk
 * around file system. All reading operations are
 * performed on the executor (async) thread.
 * @param async asynchronous operation executor.
 * @param items initial items.
 * @param cur initial directory entry.
 * @param parent initial parent directory.
 * @param errCb error callback.
 */
final class DirectoryBrowser private(
      async : Promising[Throwable],
      items : Seq[DirectoryEntry],
      cur : DirectoryView,
      parent : DirectoryView) {

  import DirectoryBrowser._

  private implicit val bindContext = Behaviour.defaultBindContext


  /** Flag indicating that this browser in the operation state. */
  private val inopState = variable(false)



  /** Current item list. */
  private val curState = variable(
    (new Viewport(items, parentAction(cur, parent)), cur))



  /** Current browser state. */
  var state = curState :< (st ⇒ st._1)



  /**
   * Enters into the specific entry.
   */
  def enter(item : DirectoryEntry) : Unit = {
    if (!item.isOwnedBy(curState.value._2))
      return
    act(load(item.open()))
  }



  /**
   * Closes/shuts down this browser.
   */
  def shutdown() : Promise[Throwable, Unit] = {
    if (inopState.value)
      return Promise.immediate(())
    val x = curState.value._2
    if (x == null)
      return Promise.immediate(())
    curState.set((new Viewport(Seq.empty, None), null))
    async {
      var c = x
      while (c != null) {
        val p = c.getParent()
        c.close()
        c = p
      }
    }
  }



  /**
   * Returns an action used to get into a parent directory.
   */
  private def parentAction(guard : DirectoryView, item : DirectoryView) : Option[() ⇒ Unit] = {
    if (item == null)
      return None

    return Some(() ⇒ {
      if (curState.value._2 `eq` guard)
        act {
          val x = load(item)
          guard.close()
          x
        }
    })
  }




  /**
   * Performs an async action.
   */
  private def act(tgt : ⇒ (Seq[DirectoryEntry], DirectoryView, DirectoryView)) : Unit = {
    if (inopState.value)
      return

    inopState set true
    async(tgt).onComplete(applyPromise)
  }



  /** Applies a new state to the browser. */
  private def applyPromise(
        res : PromiseResult[Throwable, (Seq[DirectoryEntry], DirectoryView, DirectoryView)])
      : Unit = {
    inopState set false
    res match {
      case Failure(x) ⇒ ()
      case Success((its, c, p)) ⇒
        if (curState.value._2 != null)
          curState.set((new Viewport(its, parentAction(c, p)), c))
    }
  }




  /**
   * Loads content of the specific directory.
   */
  private def load(item : DirectoryView) : (Seq[DirectoryEntry], DirectoryView, DirectoryView) = {
    val p = item.getParent()
    val items = item.listEntries().sortWith(isBefore)
    (items, item, p)
  }
}



/**
 * Directory browser companion.
 */
object DirectoryBrowser {
  /**
   * One observable position inside the file tree.
   * @param content current directory content.
   * @param parentAction optional action used to navigate into parent
   * directory.
   */
  class Viewport(
    val content : Seq[DirectoryEntry],
    val parentAction : Option[() ⇒ Unit])



  /**
   * Opers a new browser in the specified directory.
   */
  def open(async : Promising[Throwable], cur : File)
      : Promise[Throwable, DirectoryBrowser] =
    async {
      val item = DirectoryView.forFile(cur)
      val p = item.getParent()
      val items = item.listEntries()
      new DirectoryBrowser(async, items, item, p)
    }



  /** Entity sort order. */
  private def isBefore(e1 : DirectoryEntry, e2 : DirectoryEntry) : Boolean = {
    val pf1 = e1.container && !e1.filestream
    val pf2 = e2.container && !e2.filestream
    if (pf1 && pf2)
      return e1.name.compareTo(e2.name) < 0
    if (pf1 != pf2)
      pf1

    if (e1.container && e2.container)
      return e1.name.compareTo(e2.name) < 0
    if (e1.container != e2.container)
      return e1.container

    return e1.name.compareTo(e2.name) < 0
  }
}
