package ru.maxkar.ui.vfs


/** Type of file/entity in the VFS view. */
abstract sealed class FileType



/**
 * File type companion object and collection of all supported
 * file types.
 */
object FileType {
  /** Special object type - parent directory. */
  object ParentDirectory extends FileType

  /** Regular directory file. */
  object Directory extends FileType

  /**
   * Container file. This file contain other files and
   * counts as a container. Archives and disk images have
   * this file type.
   */
  object Container extends FileType

  /** Image (graphics) file. */
  object Image extends FileType

  /**
   * File of an unknown type (i.e. none of other file types).
   */
  object Unknown extends FileType
}
