package ru.maxkar.util.vfs


import java.io.File

/**
 * View of some abstract directory. Each view
 * allows you to list all entries and access
 * nested ones. Each directory keeps track of
 * system resourses associated with the virtual
 * directory and release that resources when
 * view is closed.
 * <p>Each operation except <code>close</code> and <code>isClosed</code>
 * throws <code>ViewClosedException</code> if filesystem view
 * is already closed.
 * <p>Instances of this class are not thread-safe.
 * @param parent parent directory view.
 * @param physicalPath path to a physical location
 * of view files.
 */
class DirectoryView private(
      private var parent : DirectoryView,
      physicalPath : File) {
  import DirectoryView._

  /** Flag, indicating that directory view is closed. */
  private var closed = false





  /**
   * Checks if this directory view was closed.
   */
  def isClosed() : Boolean = {
    if (closed)
      return true
    if (parent != null && parent.isClosed()) {
      closed = true
      return true
    }

    return false
  }



  /**
   * Closes a directory view and releases all
   * resources associated with it.
   */
  def close() : Unit = {
    if (isClosed())
      return
    closed = true
  }



  /**
   * Returns a parent directory view of this directory.
   * Returns null if parent does not exists or cannot be accessed.
   */
  def getParent() : DirectoryView = op {
    if (parent != null)
      return parent
    parent = findParent()
    return parent
  }



  /**
   * Tries to find a parent directory for this directory view.
   */
  private def findParent() : DirectoryView = {
    val pd = physicalPath.getParentFile()
    if (pd != null)
      return new DirectoryView(null, pd)
    return null
  }



  /**
   * Lists all directory entries inside this virtual directory.
   * @throws NonreadableDirectory if this directory view cannot
   * read entries. This may mean incompatible filesystem change
   * or filesystem created from file path instead of directory path.
   */
  def listEntries() : Seq[DirectoryEntry] = op {
    val entries = physicalPath.listFiles()
    if (entries == null)
      throw new UnreadableDirectoryException(physicalPath)
    entries.toSeq.map(
      e ⇒ new DirectoryEntry(
        this, e.getName(), isContainer(e), e.isFile()))
  }



  /**
   * Opens a nested directory view for this entry. Entry
   * must belong to this view and must denote navigable entity.
   */
  def open(entry : DirectoryEntry) : DirectoryView = {
    if (entry.parent `ne` this)
      throw new IllegalArgumentException(
        "Directory entry do not belong to this view")

    op {
      new DirectoryView(this, new File(physicalPath, entry.name))
    }
  }



  /**
   * Returns file of the entry on this view. Applicable only to
   * entries with file content (i.e. not directory).
   */
  def backingFile(entry : DirectoryEntry) : File = {
    if (entry.parent `ne` this)
      throw new IllegalArgumentException(
        "Directory entry do not belong to this view")
    op {
      new File(physicalPath, entry.name)
    }
  }



  /**
   * Performs operation on open directory view. Throws
   * ViewClosedException if view is already closed.
   * Closes all nested views as well.
   */
  private def op[T](x : ⇒ T) : T = {
    if (isClosed())
      throw new ViewClosedException()
    x
  }
}



/**
 * Directory view companion.
 */
object DirectoryView {

  /**
   * Opens a new directory view from the specific physical file.
   * @throws IllegalArgumentException if absolute file name cannot
   * be determined (i.e. file name does not exist or parent directories
   * are not accessible).
   */
  def forFile(file : File) : DirectoryView =
    new DirectoryView(null, file.getCanonicalFile())



  /**
   * Checks if file denotes a navigable container.
   */
  private def isContainer(f : File) : Boolean =
    f.isDirectory()
}
