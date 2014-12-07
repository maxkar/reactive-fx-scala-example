package ru.maxkar.util.vfs

import java.io.File

/**
 * One directory entry. Denotes abstract file or
 * directory inside virtual file system.
 * @param parent associated directory view.
 * @param name entry name.
 * @param container flag, indicating that this entry
 * is a container for other entries (i.e. directory)
 * @param filestream flag indicating that this entry has a
 * stream associated with it (i.e. it have a file content).
 */
final class DirectoryEntry private[vfs](
      private[vfs] val parent : DirectoryView,
      val name : String,
      private[vfs] val canMount :Boolean,
      private[vfs] val isDir : Boolean,
      val filestream : Boolean) {



  val container = canMount || isDir


  /** Checks if entity is owned by the parent item. */
  def isOwnedBy(view : DirectoryView) : Boolean =
    view == parent



  /** Opens this item as a directory view. */
  def open() : DirectoryView = {
    if (!container)
      throw new IllegalStateException(
        "This entry is not a container and cannot be opened")
    parent.open(this)
  }



  /** Returns a content file associated with this view. */
  def backingFile() : File =
    parent.backingFile(this)



  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[DirectoryEntry])
      return false

    val v1 = other.asInstanceOf[DirectoryEntry]

    if (this `eq` v1)
      return true
    if (v1 == null)
      return false

    return parent == v1.parent &&
      name == v1.name &&
      container == v1.container &&
      filestream == v1.filestream
  }



  override def hashCode() : Int =
    parent.hashCode() +
    name.hashCode() * 31 +
    (if (container) 31 * 31 else 0) +
    (if (filestream) 31 * 31 * 31 else 0)



  override def toString() : String =
    name
}
