package ru.maxkar.ui.vfs

import ru.maxkar.util.vfs._

/**
 * Information about one file in the file view.
 */
abstract sealed class FileInfo private[vfs]() {
  /** File name. */
  val name : String

  /** File type. */
  val fileType : FileType
}



/**
 * File information companion.
 */
object FileInfo {
  /** Parent directory file info. */
  case object ParentDirectory extends FileInfo {
    override val name = ".."
    override val fileType = FileType.ParentDirectory
  }



  /**
   * Nested file for the directory.
   */
  case class NestedItem(fileType : FileType, peer : DirectoryEntry)
      extends FileInfo {
    val name = peer.name
  }



  /** Map from file type to extension data. */
  private val EXTENSION_MAP = Map(
    "jpeg" → FileType.Image,
    "jpg" → FileType.Image,
    "png" →  FileType.Image,
    "gif" →  FileType.Image,
    "tiff" →  FileType.Image
  )



  /**
   * Creates a new file information item about nested entity.
   * File type is guessed automatically based on entity properties
   * and entity name.
   */
  def forEntity(peer : DirectoryEntry) : NestedItem =
    NestedItem(typeOf(peer), peer)



  /** Detects a file type for the directory entry. */
  def typeOf(peer : DirectoryEntry) : FileType = {
    if (peer.container)
      if (peer.filestream)
        return FileType.Container
      else
        return FileType.Directory

    val ext = getExtension(peer.name)
    if (ext == null)
      FileType.Unknown
    else
      EXTENSION_MAP.getOrElse(ext, FileType.Unknown)
  }



  /**
   * Returns an extension for the given file name. Returns
   * <code>null</code> if file name do not have any extensions.
   */
  private def getExtension(name : String) : String = {
    val dotIdx = name.lastIndexOf(".")
    /* Not a typo, skip linux hidden files. */
    if (dotIdx <= 0)
      null
    else
      name.substring(dotIdx + 1).toLowerCase()
  }
}
